use std::borrow::Cow;
use std::fmt::Debug;
use std::mem::replace;
use std::time::Duration;
use pi_time::Instant;
use std::{collections::HashSet, io::Result as IoResult};

use pi_futures::BoxFuture;
use pi_async::prelude::{AsyncRuntime, AsyncValue};
use fixedbitset::FixedBitSet;
use pi_slotmap::{SlotMap, DefaultKey};
use thiserror::Error;

use pi_async_graph::{async_graph, Runnble, Runner};
use pi_graph::{DirectedGraph, DirectedGraphNode, NGraph, NGraphBuilder};
use bevy_ecs::{
	query::Access,
	archetype::ArchetypeComponentId,
	world::World,
};
use pi_share::{ShareMutex, ThreadSync, ThreadSend, Share};
use flume::{Receiver, bounded};

#[derive(Default)]
pub struct DispatcherMgr {
	arr: SlotMap<DefaultKey, Box<dyn Dispatcher>>,
	// running: AtomicBool, // true代表有派发器正在运行，false代表
	pre_wait: ShareMutex<Option<Receiver<()>>>,
}

impl DispatcherMgr {
	pub fn insert(&mut self, value: Box<dyn Dispatcher>) -> DefaultKey {
		self.arr.insert(value)
	}

	pub fn remove(&mut self, key: DefaultKey) {
		self.arr.remove(key);
	}

	pub fn run<'a>(&'a self, key: DefaultKey, is_wait: bool) -> BoxFuture<'a, ()> {
		Box::pin(async move {
			if let Some(r) = self.arr.get(key) {
				let (sender, receiver) = bounded::<()>(1);
				let pre_await = {
					let mut lock = self.pre_wait.lock();
					if !is_wait && lock.is_some() {// 
						return;
					}
					replace(&mut *lock, Some(receiver))
				};
				if let Some(pre_wait) = pre_await {
					let _ = pre_wait.recv_async().await;
				}
				r.run().await;
				if !is_wait {
					let mut lock = self.pre_wait.lock();
					*lock = None;
				}
				let _ = sender.send_async(()).await;
			}
		})
	}
}


/// 派发器 接口
pub trait Dispatcher: ThreadSync + 'static {
    /// 只有 run 方法
    fn run<'a>(&'a self) -> BoxFuture<'a, ()>;
}

/// 串行 派发器
pub struct SingleDispatcher<A:  AsyncRuntime<()>>
{
    /// 异步运行时
    rt: A,
    /// 派发器 包含 一组 Stage
    vec: Share<Vec<Stage>>,
}

impl<A: AsyncRuntime<()>> SingleDispatcher<A>
{
    pub fn init(&mut self, vec: Vec<Stage>, arrange: &World) {
        let mut v1 = Vec::new();
        for i in vec.into_iter() {
            v1.push(i);

            // arrange node
            if let Some(node) = arrange.arrange() {
                let mut stage = StageBuilder::new();
                stage.add_node(node);

                v1.push(Share::new(stage.build(arrange)))
            }
        }
		self.vec =  Share::new(v1);
    }

	pub fn new(rt: A) -> Self {
        SingleDispatcher {
            vec: Share::new(Vec::new()),
            rt,
        }
    }

    /// 执行指定阶段的指定节点
    pub fn exec(
        vec: Share<Vec<Stage>>,
		statistics: Share<ShareMutex<Vec<(Cow<'static, str>, Duration)>>>,
        rt: A,
        mut stage_index: usize,
        mut node_index: usize,
		wait: AsyncValue<()>,
		has_async: bool,
    ) {
        while stage_index < vec.len() {
            let g = &vec[stage_index];
            let arr = g.topological_sort();
            if node_index >= arr.len() {
                // stage结束，apply
                for elem in arr {
                    let node = g.get(elem).unwrap().value();
                    node.apply();
                }
                stage_index += 1;
                node_index = 0;

				// if stage_index == vec.len()  {
				// 	for i in statistics.lock().unwrap().iter() {
				// 		// log::warn!("{:?}", i);
				// 		println!("{:?}", i);
				// 	}
				// }
                continue;
            }
            let node = g.get(&arr[node_index]).unwrap().value();
            node_index += 1;
            if let Some(sync) = node.is_sync() {
                if sync {
					let t = Instant::now();
					// println!("start1======");
                    node.get_sync().run();
					statistics.lock().push((node.name(), Instant::now() - t));
					// println!("end1======");
                } else {
                    let f = node.get_async();
                    let vec1 = vec.clone();
                    let rt1 = rt.clone();
					let name = node.name();
                    rt.spawn(rt.alloc(), async move {
						// println!("start======");
						let t = Instant::now();
                        f.await.unwrap();
						statistics.lock().push((name, Instant::now() - t));
						// println!("end======");
                        SingleDispatcher::exec(vec1, statistics, rt1, stage_index, node_index, wait, true);
                    })
                    .unwrap();

                    return;
                }
            }
        }

		// 派发器执行完毕, 用wait派发一个任务，试await触发
		if has_async {
			wait.set(());
		} else {
			rt.spawn(rt.alloc(), async move {wait.set(())}).unwrap();
		}
    }
}


impl<A: AsyncRuntime<()>> Dispatcher for SingleDispatcher<A>
{
    /// 同步节点自己执行， 如果有异步节点，则用单线程运行时执行
    fn run<'a>(&'a self) -> BoxFuture<'a, ()> {
		Box::pin(async move {
			let statistics = Share::new(ShareMutex::new(Vec::new()));
			let wait = pi_async::prelude::AsyncValue::new();
			Self::exec(self.vec.clone(),  statistics, self.rt.clone(), 0, 0, wait.clone(), false);
			wait.await;
		})
    }
}
pub struct MultiDispatcher<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>>(Share<MultiInner<A1, A2>>);

impl<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>> MultiDispatcher<A1, A2>
{
    pub fn new(
        vec: Vec<(Stage, Option<A2>)>,
        multi: A1,
    ) -> Self {
        MultiDispatcher(Share::new(MultiInner::new(vec, multi)))
    }
}


impl<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>> Dispatcher for MultiDispatcher<A1, A2>
{
    /// 根据阶段是单线程还是多线程，
    /// 如果多线程阶段，同步节点和异步节点，则用多线程运行时并行执行
    /// 如果单线程阶段，同步节点自己执行， 如果有异步节点，则用单线程运行时执行
    /// 一般为了线程安全，第一个阶段都是单线程执行
    fn run<'a>(&'a self) -> BoxFuture<'a, ()> {
        Box::pin(async move {
			let c = self.0.clone();
			// 没有任务，直接返回
			if c.vec.len() == 0 {
				return;
			}
			let wait = pi_async::prelude::AsyncValue::new();
			exec(c, 0, wait.clone());
			wait.await;
		})
    }
}

struct MultiInner<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>>
{
    vec: Vec<(Stage, Option<A2>)>,
    multi: A1,
}

impl<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>> MultiInner<A1, A2>
{
    pub fn new(
        vec: Vec<(Stage, Option<A2>)>,
        multi: A1,
    ) -> Self {
        MultiInner { vec, multi }
    }
}

/// 执行指定阶段
fn exec<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>>(d: Share<MultiInner<A1, A2>>, stage_index: usize, wait: AsyncValue<()>)
{
    if stage_index >= d.vec.len() {
		wait.set(());
        return;
    }
    if let Some(single) = &d.vec[stage_index].1 {
        let s = single.clone();
        single_exec(d, stage_index, 0, s, wait);
    } else {
        multi_exec(d, stage_index, wait);
    }
}

/// 单线程执行, 尽量本线程运行，遇到异步节点则用单线程运行时运行
fn single_exec<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>>(
    d: Share<MultiInner<A1, A2>>,
    stage_index: usize,
    mut node_index: usize,
    single: A2,
	wait: AsyncValue<()>,
)
{
    let g = &d.vec[stage_index].0;
    let single1 = single.clone();
    loop {
        let arr = g.topological_sort();
        if node_index >= g.node_count() {
            // stage结束，apply
            for elem in arr {
                let node = g.get(elem).unwrap().value();
                node.apply();
            }

            // 本阶段执行完毕，执行下一阶段
            return exec(d, stage_index + 1, wait);
        }

        let node = g.get(&arr[node_index]).unwrap().value();
        node_index += 1;
        if let Some(sync) = node.is_sync() {
            if sync {
                if stage_index > 0 && node_index == 1 {
                    let f = node.get_sync();
                    let d1 = d.clone();
                    single1
                        .spawn(single1.alloc(), async move {
                            f.run();
                            single_exec(d1, stage_index, node_index, single, wait);
                        })
                        .unwrap();
                    return;
                }
                // 如果是最开始的阶段， 或者非起始节点，则立即同步执行
                node.get_sync().run();
            } else {
                let f = node.get_async();
                let d1 = d.clone();
                single1
                    .spawn(single1.alloc(), async move {
                        let _ = f.await;
                        single_exec(d1, stage_index, node_index, single, wait);
                    })
                    .unwrap();
                return;
            }
        }
    }
}

/// 多线程执行
fn multi_exec<A1: AsyncRuntime<()>, A2: AsyncRuntime<()>>(d: Share<MultiInner<A1, A2>>, stage_index: usize, wait: AsyncValue<()>)
{
    let d1 = d.clone();
    d.multi
        .spawn(d.multi.alloc(), async move {
            let g = &d1.vec[stage_index].0;
            let r = async_graph(d1.multi.clone(), g.clone()).await;
            if r.is_ok() {
                // stage结束，apply
                let arr = g.topological_sort();
                for elem in arr {
                    let node = g.get(elem).unwrap().value();
                    node.apply();
                }

                exec(d1, stage_index + 1, wait);
            }
        })
        .unwrap();
}

/// 图 的 节点
#[derive(Clone)]
pub struct GraphNode {
    // 节点id，每个节点有 独一无二的 id
    pub(crate) id: usize,
    // // 节点的输入，意义是 它 依赖 的 节点，决定 执行关系
    // pub(crate) reads: Vec<usize>,
    // // 节点的输出，意义是 依赖 它 的 节点，决定 执行关系
    // pub(crate) writes: Vec<usize>,
	pub(crate) access: Access<ArchetypeComponentId>,
    // 执行节点
    pub(crate) node: ExecNode,

	pub(crate) label: String,
}

/// 操作
pub trait Operate: ThreadSend + 'static {
    /// 返回类型
    type R;

    /// 执行
    /// 执行结果，会缓冲到 内部，等当前Stage全部执行结束后，再统一调用apply，刷新到world上
    fn run(&self, world: &mut World) -> Self::R;

    /// 应用
    /// 在该stage所有的system run 结束之后 执行
    /// 扫描所有的system，将当前缓冲的数据 刷新到 world 上
    fn apply(&self, world: &mut World);

	fn name(&self) -> Cow<'static, str>;
}

/// 同步的run方法
#[derive(Clone)]
pub struct Run(pub(crate) Share<dyn Operate<R = ()>>);
unsafe impl Send for Run {}

impl Runner for Run {
    fn run(self) {
        self.0.run()
    }
}

/// 异步的run方法
#[derive(Clone)]
pub struct AsyncRun(pub (crate) Share<dyn Operate<R = BoxFuture<'static, IoResult<()>>>>);
unsafe impl Send for AsyncRun {}

/// 执行节点
#[derive(Clone)]
pub enum ExecNode {
    /// 不执行任何操作
    None(&'static str),
    /// 同步函数
    Sync(Run),
    /// 异步函数
    Async(AsyncRun),
}

impl Debug for ExecNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&*self.name()).finish()
    }
}

unsafe impl Sync for ExecNode {}

impl Runnble for ExecNode {
    type R = Run;

    fn is_sync(&self) -> Option<bool> {
        match self {
            ExecNode::None(_) => None,
            ExecNode::Sync(_) => Some(true),
            _ => Some(false),
        }
    }

    /// 获得需要执行的同步函数
    fn get_sync(&self) -> Run {
        match self {
            ExecNode::Sync(f) => Run(f.0.clone()),
            _ => panic!(),
        }
    }

    /// 获得需要执行的异步块
    fn get_async(&self) -> BoxFuture<'static, IoResult<()>> {
        match self {
            ExecNode::Async(f) => f.0.run(),
            _ => panic!(),
        }
    }
}

impl ExecNode {
    fn apply(&self) {
        match self {
            ExecNode::Sync(f) => f.0.apply(),
            ExecNode::Async(f) => f.0.apply(),
            _ => (),
        };
    }

	fn name(&self) -> Cow<'static, str> {
		match self {
            ExecNode::Sync(f) => f.0.name(),
            ExecNode::Async(f) => f.0.name(),
            ExecNode::None(n) => Cow::from(*n),
        }
	}
}

pub trait Arrange {
    fn arrange(&self) -> Option<GraphNode>;
}

/// Stage 是 由 可执行节点 组成的 图
type Stage = Share<NGraph<usize, ExecNode>>;

/// 阶段构造器
#[derive(Default)]
pub struct StageBuilder {
    // 所有的节点 id
    components: HashSet<usize>,
    // 节点
    systems: Vec<GraphNode>,
    // 边，(输入节点id, 输出节点id)
    edges: Vec<(usize, usize)>,
}

impl StageBuilder {
    /// 创建
    pub fn new() -> Self {
        StageBuilder::default()
    }

    /// 加入节点
    pub fn add_node<T: Into<GraphNode>>(&mut self, node: T) -> &mut Self {
        let node = node.into();

        // 加入 System
        self.systems.push(node);

        self
    }

	/// 取到刚添加的最后一个节点
	pub fn get_last_node(&self) -> Option<&GraphNode> {
		let len = self.systems.len();
		if len > 0 {
			Some(&self.systems[len - 1])
		} else {
			None
		}
	}

    /// 显示指定 节点的依赖 关系
    pub fn order(mut self, before: usize, after: usize) -> Self {
        // 添加边: before --> after
        self.edges.push((before, after));

        self
    }

    /// 构建 拓扑 序
    pub fn build(mut self, world: &World) -> NGraph<usize, ExecNode> {
        // Stages --> NGraph
        let mut builder = NGraphBuilder::new();

		for s in self.systems.iter() {
			match write_depend(world, s.access.get_reads_and_writes(), s.access.get_writes(), s.access.get_modify()) {
				Ok((mut r, w)) => {
					r.difference_with(&w);

					// log::warn!("system: {:?}", s.node.name());
					// 边: 该节点 --> 输出
					for k in r.ones() {
					    self.components.insert(k);
					    self.edges.push((k, s.id));
						// log::warn!("read: {:?}", &world.archetypes().archetype_component_info[k]);
					}

					for k in w.ones() {
					    self.components.insert(k);
					    self.edges.push((s.id, k));
						// log::warn!("write: {:?}", &world.archetypes().archetype_component_info[k]);
					}
				},
				Err(c) => {
					let c: Vec<&'static str> = c.ones().map(|i| {(*&world.archetypes().archetype_component_info[i]).clone()}).collect();
					panic!("{:?}", BuildErr::WriteConflict(s.label.clone(), c));
				}
			}
		}

        for id in self.components {
            // 每个 Component 都是一个节点
            builder = builder.node(id, ExecNode::None(world.archetypes().archetype_component_info[id]));
        }

        for n in self.systems {
            // 每个 System 都是 一个 可执行节点
            builder = builder.node(n.id, n.node);
        }

        for n in self.edges {
            // 边 对应 Graph 的 边
            builder = builder.edge(n.0, n.1);
        }

		builder.build().unwrap()
    }
}

#[derive(Debug, Error)]
pub enum BuildErr {
	#[error("build fail, node is circly: {0:?}")]
	Circly(Vec<usize>),
	#[error("build fail, write conflict, system: {0:?}, write access {1:?}")]
	WriteConflict(String, Vec<&'static str>)
}


fn write_depend(w: &World, read_writes: &FixedBitSet, writes: &FixedBitSet, modifys: &FixedBitSet) -> Result<(FixedBitSet, FixedBitSet), FixedBitSet> {
	Ok((read_writes, writes))
	// let (mut read_writes_new, mut write_new) = (read_writes.clone(), writes.clone());
	// let conflict = FixedBitSet::default();

	// if conflict.count_ones(..) > 0 {
	// 	println!("len: {:?}, {:?}", conflict.count_ones(..), conflict);
	// 	Err(conflict)
	// } else {
	// 	Ok((read_writes_new, write_new))
	// }
}