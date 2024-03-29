use std::intrinsics::transmute;
use std::slice::Iter;
use std::{marker::PhantomData, any::TypeId};

use pi_dirty::{LayerDirty as LayerDirty1, DirtyIterator, ReverseDirtyIterator, PreDirty, NextDirty};
use pi_ecs::monitor::Delete;
use pi_ecs::prelude::{AddedFetch, ModifyedFetch, DeletedFetch};
use pi_ecs_macros::all_tuples;
use pi_hash::XHashMap;
use pi_map::Map;

use pi_ecs::prelude::{SystemParamFetch};
use pi_ecs::{
	component::{Component, ComponentId},
	world::World, 
	sys::{
		system::SystemState, 
		param::{SystemParamState}
	},
	storage::{SecondaryMap, Local}, 
	monitor::{Event, ComponentListen, Create, Modify, Listen, Listeners,ListenSetup},
	prelude::{FetchState, Fetch, WorldQuery, FilterFetch, ChangedFetch, Or, OrFetch}, entity::Id, archetype::{ArchetypeIdent},
};
use pi_ecs::sys::param::{assert_component_access_compatibility, SystemParam};
use pi_slotmap_tree::{RecursiveIterator, Storage};

use crate::prelude::Layer;

use super::tree::{IdtreeState, EntityTree};
use pi_share::Share;

/// 层脏
/// 默认监听了组件的修改、创建事件、Tree的创建事件，当监听到这些事件，会添加到层次脏列表
pub struct LayerDirty<'s, A: ArchetypeIdent, F: WorldQuery> {
	state: &'s mut LayerDirtyState1<A, F>,
	entity_tree: EntityTree<'s, A>,
}

impl<'s, A: ArchetypeIdent, F: WorldQuery> LayerDirty<'s, A, F> {
	fn new(world: &World, state: &'s mut LayerDirtyState<A, F>, system_state: &SystemState,  last_change_tick: u32, change_tick: u32) -> Self {
			unsafe{ state.0.inner_fetch.setting(world, last_change_tick, change_tick)};
			Self {
				state: &mut state.0,
				entity_tree: unsafe{ IdtreeState::<A>::get_param(state.1.as_mut(), system_state, world, 0) },
			}
		}
}

impl<'s, A, F> SystemParam for LayerDirty<'s, A, F>
where
	A: ArchetypeIdent,
	F: WorldQuery + 'static,
    F::Fetch: FilterFetch + InstallLayerListen,
{
    type Fetch = LayerDirtyState<A, F>;
}

impl<'s, A: ArchetypeIdent, F: WorldQuery> LayerDirty<'s, A, F> {
	/// 返回一个自动迭代器
	pub fn iter(&mut self) -> AutoLayerDirtyIter<A, F> {
		AutoLayerDirtyIter {
			matchs: self.state.is_matchs,
			iter_inner: self.state.layer_inner.layer_list.iter(),
			mark_inner: &mut self.state.layer_inner.dirty_mark,
			pre_iter: None,
			tree: &self.entity_tree,
			// archetype_id: state.archetype_id,
			mark: PhantomData,
		}
	}

	/// 返回一个手动迭代器
	pub fn iter_manual(&mut self) -> ManualLayerDirtyIter<A, F> {
		ManualLayerDirtyIter {
			matchs: self.state.is_matchs,
			iter_inner: self.state.layer_inner.layer_list.iter(),
			mark_inner: &mut self.state.layer_inner.dirty_mark,
			tree: &self.entity_tree,
			// archetype_id: state.archetype_id,
			mark: PhantomData,
		}
	}

	pub fn count(&self) -> usize {
		self.state.layer_inner.layer_list.count()
	}

	pub fn start(&self) -> usize {
		self.state.layer_inner.layer_list.start()
	}

	pub fn end(&self) -> usize {
		self.state.layer_inner.layer_list.end()
	}

	pub fn split(&mut self, layer: usize) -> (RemainDirty<A>, OutDirty<A>) {
		let s = self.state.layer_inner.layer_list.split(layer);
		(RemainDirty(s.0), OutDirty(s.1, self.state.archetype_id, &mut self.state.layer_inner.dirty_mark))
	}

	pub fn iter_reverse(&mut self) -> LayerReverseDirtyIter<A, F> {
		LayerReverseDirtyIter {
			matchs: self.state.is_matchs,
			iter_inner: self.state.layer_inner.layer_list.iter_reverse(),
			mark_inner: &mut self.state.layer_inner.dirty_mark,
			tree: &self.entity_tree,
			// archetype_id: state.archetype_id,
			mark: PhantomData,
		}
	}
}

pub struct OutDirty<'a, A: ArchetypeIdent>(NextDirty<'a, Id<A>>, Local, &'a mut SecondaryMap<Id<A>, usize>);
pub struct RemainDirty<'a, A: ArchetypeIdent>(PreDirty<'a, Id<A>>);

impl<'a, A: ArchetypeIdent> OutDirty<'a, A> {
	pub fn iter(&'a mut self) -> OutDirtyIter<'a, A> {
		let i = self.0.iter();
		OutDirtyIter(i, self.2)
	}	
}

pub struct OutDirtyIter<'a, A: ArchetypeIdent>(Iter<'a, Id<A>>, &'a mut SecondaryMap<Id<A>, usize>);

impl<'a, A: ArchetypeIdent> Iterator for OutDirtyIter<'a, A>{
    type Item = Id<A>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
		match self.0.next() {
			Some(r) => {
				self.1.remove(r); // 标记为不脏
				Some(r.clone())
			},
			None => None
		}
	}
}

impl<'a, A: ArchetypeIdent> RemainDirty<'a, A>{
    pub fn mark(&mut self, id: Id<A>, layer: usize) {
        self.0.mark(id, layer);
    }

    pub fn delete(&mut self, id: Id<A>, layer: usize) {
        self.0.delete(id, layer);
    }
}

pub struct AutoLayerDirtyIter<'s, A: ArchetypeIdent, F: WorldQuery> {
	mark: PhantomData<&'s (A, F)>,
	matchs: bool,
	iter_inner: DirtyIterator<'s, Id<A>>,

	mark_inner: &'s mut SecondaryMap<Id<A>, usize>,

	tree: &'s EntityTree<'s, A>,
	// archetype_id: Local,

	pre_iter: Option<RecursiveIterator<'s, Id<A>, &'s IdtreeState<A>>>,
	// layers: &'s mut  ReadFetch<C>,
}

impl<'s, A: ArchetypeIdent, F: WorldQuery> Iterator for AutoLayerDirtyIter<'s, A, F>
where
	F::Fetch: FilterFetch,
{
    type Item = Id<A>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
		if !self.matchs {
			return  None;
		}
		if let Some(r) = &mut self.pre_iter {
			// 上次迭代的脏还没完成，继续迭代
			match r.next() {
				Some(next) => {
					self.mark_inner.remove(&next); // 标记为不脏
					return Some(next)
				},
				None => self.pre_iter = None
			};
		}

		// 上一个子树迭代完成，继续迭代下一个脏
		let item = self.iter_inner.next();
		if let Some((local, layer)) = item {
			if let Some(layer1) = self.mark_inner.get(local) {
				let layer1 = *layer1;
				self.mark_inner.remove(local); // 标记为不脏

				// 记录的层次和实际层次相等，并且在idtree中的层次也相等，则返回该值
				if layer == layer1{
					if let Some(r) = self.tree.get_layer(*local) {
						if r.layer() == layer {
							// 是否判断changed？TODO
							// 记录上次迭代出的实体id，下次将对该节点在itree进行先序迭代
							if let Some(down) = self.tree.get_down(*local) {
								let head = down.head;
								self.pre_iter = Some(self.tree.recursive_iter(head));
							}
							return Some(*local);
						}
					}
				}
			}
		}
		return None;
    }
}

/// 手动迭代器（需要自己控制脏标记）
pub struct ManualLayerDirtyIter<'s, A: ArchetypeIdent, F: WorldQuery> {
	mark: PhantomData<&'s (A, F)>,
	matchs: bool,
	iter_inner: DirtyIterator<'s, Id<A>>,

	mark_inner: &'s mut SecondaryMap<Id<A>, usize>,

	tree: &'s EntityTree<'s, A>,
}

impl<'s, A: ArchetypeIdent, F: WorldQuery> Iterator for ManualLayerDirtyIter<'s, A, F>
where
	F::Fetch: FilterFetch,
{
    type Item = (Id<A>, &'s mut SecondaryMap<Id<A>, usize>, usize);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
		if !self.matchs {
			return  None;
		}

		// 上一个子树迭代完成，继续迭代下一个脏
		loop {
			let item = self.iter_inner.next();
			if let Some((local, layer)) = item {
				if let Some(layer1) = self.mark_inner.get(local) {
					let layer1 = *layer1;

					// 记录的层次和实际层次相等，并且在idtree中的层次也相等，则返回该值
					if layer == layer1{
						if let Some(r) = self.tree.get_layer(local.clone()) {
							if r.layer() == layer {
								return Some((local.clone(), unsafe { transmute(self.mark_inner as *mut SecondaryMap<Id<A>, usize> as usize as *mut SecondaryMap<Id<A>, usize>) }, r.layer()));
							}
						}
					}
				}
			} else {
				return None;
			}
		}
    }
}

/// 逆序迭代，从叶子节点向父迭代
pub struct LayerReverseDirtyIter<'s, A: ArchetypeIdent, F: WorldQuery> {
	mark: PhantomData<&'s (A, F)>,
	matchs: bool,
	iter_inner: ReverseDirtyIterator<'s, Id<A>>,

	mark_inner: &'s mut SecondaryMap<Id<A>, usize>,

	tree: &'s EntityTree<'s, A>,
}

impl<'s, A: ArchetypeIdent, F: WorldQuery> Iterator for LayerReverseDirtyIter<'s, A, F>
where
	F::Fetch: FilterFetch,
{
    type Item = Id<A>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
		if !self.matchs {
			return  None;
		}

		let item = self.iter_inner.next();
		if let Some((local, layer)) = item {
			if let Some(layer1) = self.mark_inner.get(local) {
				let layer1 = *layer1;

				// 记录的层次和实际层次相等，并且在idtree中的层次也相等，则返回该值
				if layer == layer1{
					if let Some(r) = self.tree.get_layer(local.clone()) {
						if r.layer() == layer {
							// 是否判断changed？TODO
							// 记录上次迭代出的实体id，下次将对该节点在itree进行先序迭代
							return Some(local.clone());
						}
					}
				}
			}
		}
		return None;
	}
}

pub struct LayerDirtyInner<A: ArchetypeIdent>{
	pub(crate) layer_list: LayerDirty1<Id<A>>, // 脏列表
	pub(crate) dirty_mark: SecondaryMap<Id<A>, usize>,
	// 已经安装过监听器的组件不需要再安装（用于记录已安装监听器的组件的ComponentId）
	pub(crate) is_install: XHashMap<ComponentId, ()>,
}

impl<A: ArchetypeIdent> LayerDirtyInner<A> {
	pub fn new() -> Self {
		Self {
			layer_list: LayerDirty1::default(),
			dirty_mark: SecondaryMap::with_capacity(0),
			is_install: XHashMap::default(),
		}
	}
	pub fn insert(&mut self, id: Id<A>, tree: &IdtreeState<A>) {
		match tree.get_layer(id) {
            Some(r) => {
                if r.layer() != 0 {
					let d = match self.dirty_mark.get_mut(&id) {
						Some(r) => r,
						None => {
							// 如果dirty_mark中不存在id，需要新创建
							self.dirty_mark.insert(id, 0);
							&mut self.dirty_mark[id]
						},
					};
					// 新的layer和旧的layer不相等，则记录新的（不删除原来的，在迭代层次脏时，会重现判断层，原有的会自动失效）
                    if *d != r.layer() {
                        *d = r.layer();
                        self.layer_list.mark(id, r.layer());
                    }
                }
            }
            _ => (),
        };
	}
}

struct ShareIdtreeState<A: ArchetypeIdent>(Share<IdtreeState<A>>);

impl<A: ArchetypeIdent> Clone for  ShareIdtreeState<A>{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

unsafe impl<A: ArchetypeIdent> Send for ShareIdtreeState<A> {}
unsafe impl<A: ArchetypeIdent> Sync for ShareIdtreeState<A> {}

pub trait InstallLayerListen: FilterFetch {
	/// 安装监听器，收到事件设脏
	/// 安全：
	///  * 保证layer在监听器删除之前，该指针不会被销毁
	unsafe fn install<A: ArchetypeIdent>(&self, world: &mut World, layer: *const LayerDirtyInner<A>, idtree: *const IdtreeState<A>, state: &<Self as Fetch>::State);
}

macro_rules! impl_install {
    ($name: ident, $listen: ty) => {
		impl<T: Component> InstallLayerListen for $name<T>{
	
			unsafe fn install<A: ArchetypeIdent>(&self, world: &mut World, layer: *const LayerDirtyInner<A>, idtree: *const IdtreeState<A>, state: &<Self as Fetch>::State) {
				let layer = layer as usize;
				let layer_obj = &mut *(layer as *mut LayerDirtyInner<A>);
				let idtree = idtree as usize;
				if let None = layer_obj.is_install.get(&state.component_id) {
					// let component_id = state.component_id;
		
					// 安装监听器，监听对应组件修改，并将改变的实体插入到脏列表中
					let listen = move |
						event: Event, 
						_:Listen<ComponentListen<A, T, $listen>>,
						// layers: Query<A, &C>
					| {
						
						// 标记层脏
						(&mut *(layer as *mut LayerDirtyInner<A>)).insert(Id::new(event.id.local()) , &*(idtree as *const IdtreeState<A>));
					};
					// 标记监听器已经设置，下次不需要重复设置（同一个查询可能涉及到多次相同组件的过滤）
					// layer_obj.is_install.insert(component_id, ());
					let l = listen.listeners();
					l.setup(world);
				}
			}
		}
	}
}

impl_install!(ChangedFetch, (Create, Modify));
impl_install!(AddedFetch, Create);
impl_install!(ModifyedFetch, Modify);
impl_install!(DeletedFetch, Delete);

macro_rules! impl_query_listen_tuple {
    ($(($filter: ident, $state: ident)),*) => {
        #[allow(unused_variables)]
        #[allow(non_snake_case)]
        impl<$($filter: InstallLayerListen),*> InstallLayerListen for ($($filter,)*) {
			unsafe fn install<A>(&self, world: &mut World, layer: *const LayerDirtyInner<A>, idtree: *const IdtreeState<A>, state: &<Self as Fetch>::State) where 
				A: ArchetypeIdent {
				let ($($filter,)*) = self;
				let ($($state,)*) = state;
				$($filter.install::<A>(world, layer, idtree.clone(), $state);)*
			}
        }

		#[allow(unused_variables)]
        #[allow(non_snake_case)]
		impl< $($filter: InstallLayerListen),*> InstallLayerListen for Or<($(OrFetch<$filter>,)*)> {
			unsafe fn install<A>(&self, world: &mut World, layer: *const LayerDirtyInner<A>, idtree: *const IdtreeState<A>, state: &<Self as Fetch>::State)
			where 
				A: ArchetypeIdent {
				let ($($filter,)*) = &self.0;
				let ($($state,)*) = &state.0;
				$($filter.fetch.install::<A>(world, layer, idtree.clone(), $state);)*
			}
		}
    };
}
all_tuples!(impl_query_listen_tuple, 0, 15, F, S);

pub struct LayerDirtyState<A: ArchetypeIdent, F: WorldQuery> (LayerDirtyState1<A, F>, Box<IdtreeState<A>>);

// pub(crate) tree_state: IdtreeState<A>,
pub struct LayerDirtyState1<A: ArchetypeIdent, F: WorldQuery> {
	pub(crate) layer_inner: Box<LayerDirtyInner<A>>, // 脏列表
	pub(crate) inner_state: F::State,
	pub(crate) inner_fetch: F::Fetch,
	// pub(crate) layer_fetch: ReadFetch<C>,

	pub(crate) is_matchs: bool,
	_world: World, // 抓住索引，确保在其销毁之前，World不销毁
	pub(crate) archetype_id: Local,
	mark: PhantomData<A>,
}

unsafe impl<A: ArchetypeIdent, F: WorldQuery + 'static> SystemParamState for LayerDirtyState<A, F>
	where F::State: FetchState, F::Fetch: FilterFetch + InstallLayerListen{
    type Config = ();
	
	/// 检查数据访问冲突
	/// 一些状态的初始化
	/// 添加监听器监听数据的改变，进行脏设置
    fn init(world: &mut World, system_state: &mut SystemState, _config: Self::Config) -> Self {
		let (last_change_tick, change_tick) = (world.last_change_tick(), world.change_tick());
		let mut archetype_component_access = Default::default();
		let archetype_id = world.archetypes_mut().get_or_create_archetype::<A>();
		let state = F::State::init(world, 0, archetype_id);

		// let layer_state = ReadState::<C>::init(world, 0);
		let tree_state = IdtreeState::<A>::init(world, system_state, ()) ;

		let mut fetch = unsafe { F::Fetch::init(world, &state) };
		// let mut layer_fetch = unsafe { ReadFetch::<C>::init(world, &layer_state) };
		let archetype_id = match world.archetypes().get_id_by_ident(TypeId::of::<A>()) {
			Some(r) => r.clone(),
			None => panic!(),
		};

		let archetypes = world.archetypes();
		let archetype = &archetypes[archetype_id];

		let is_matchs = state.matches_archetype(archetype);
		
		if is_matchs{
			unsafe{
				fetch.set_archetype(&state, archetype, world);
				// layer_fetch.set_archetype(&layer_state, archetype, world);
				
				fetch.setting(world, last_change_tick, change_tick);
				// layer_fetch.setting(world, last_change_tick, change_tick);
				state.update_archetype_component_access(archetype, &mut archetype_component_access);
				state.update_archetype_component_access(archetype, &mut archetype_component_access);
				// layer_state.update_archetype_component_access(archetype, &mut archetype_component_access);
			}
		}

		let r = LayerDirtyState(LayerDirtyState1 {
			layer_inner: Box::new(LayerDirtyInner::new()),
			inner_state: state,
			inner_fetch: fetch,
			// layer_state,
			// layer_fetch,
			mark: PhantomData,
			_world: world.clone(),
			archetype_id,
			is_matchs
        }, Box::new(tree_state));
		// unsafe { IdtreeState::<A>::get_param(&mut tree_state, system_state, world, 0) }
		// // 判断访问是否冲突
		// let tree_archetype_id = world.archetypes().get_archetype_resource_id::<Idtree<A, N>>().unwrap().clone();
		// if archetype_component_access.has_write(tree_archetype_id) {
		// 	panic!("systemparam init fail, {:?} read and write conflict, in system {:?}", std::any::type_name::<Idtree<A, N>>(), &system_state.name);
		// }
		// component_access.add_read(tree_resoruce_id);
		// archetype_component_access.add_read(tree_archetype_id);

		assert_component_access_compatibility(
            &system_state.name(),
            std::any::type_name::<LayerDirty::<A, F>>(),
            std::any::type_name::<LayerDirty::<A, F>>(),
            system_state.archetype_component_access(),
            &archetype_component_access,
            world,
        );
		// 将查询访问的原型组件放入系统的原型组件集中（用于检查系统与系统的访问组件是否冲突，访问不同原型的同类型组件是允许的）
        system_state
            .archetype_component_access_mut().combined_access_mut()
            .extend(&archetype_component_access.access());

		if is_matchs {
			let inner = r.0.layer_inner.as_ref() as *const LayerDirtyInner<A>;
			let state = unsafe{&*( &r.0.inner_state as *const F::State) };
			let tree_state = r.1.as_ref() as *const IdtreeState<A>;

			// 监听
			unsafe {InstallLayerListen::install::<A>(&r.0.inner_fetch, world, inner, tree_state, state)};
			
			let inner = inner as usize;
			let tree_state1 = tree_state as usize;
			// 监听Layer组件（泛型C，实现了GetLayer trait）
			let listen = move |
				event: Event,
				_: Listen<ComponentListen<A, Layer<A>, Modify>>,
				// layers: Query<A, &C>,
			| {
				// 标记层脏
				unsafe{&mut *(inner as *mut LayerDirtyInner<A>)}.insert(unsafe { Id::new(event.id.local())}, unsafe{&*(tree_state1 as *const IdtreeState<A>)});
			};
			let l = listen.listeners();
			l.setup(world);
		}

		r
    }

	fn apply(&mut self, _world: &mut World) {
		// 清理脏记录
		self.0.layer_inner.layer_list.clear();
	}

    fn default_config() {}
}

impl<'w, 's, A: ArchetypeIdent, F: WorldQuery + 'static> SystemParamFetch<'w, 's> for LayerDirtyState<A, F>
	where 
		F::State: FetchState,
		F::Fetch: InstallLayerListen + FilterFetch{
    type Item = LayerDirty<'s, A, F>;

    #[inline]
    unsafe fn get_param(
        state: &'s mut Self,
        system_state: &SystemState,
        world: &'w World,
        change_tick: u32,
    ) -> Self::Item {
		LayerDirty::new(world, state, system_state, system_state.last_change_tick(), change_tick)
    }
}