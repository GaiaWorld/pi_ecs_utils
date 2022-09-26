/// 测试系统参数: Idtree
/// idtree包含隐试条件，即：world中已经注册了Layer,NodeUp,NodeDown这三种组件, 否则参数fetch将失败

use std::sync::Arc;

use pi_ecs::{prelude::{Id, Query, IntoSystem, StageBuilder, SingleDispatcher, Dispatcher, Res, System}, world::World, storage::Offset};
use pi_async::{rt::{AsyncRuntimeBuilder}, prelude::MultiTaskRuntime};
use pi_ecs_utils::prelude::{Layer, NodeDown, NodeUp, EntityTreeMut, EntityTree};
use pi_null::Null;

#[derive(Default)]
pub struct Node;

#[derive(Debug)]
/// 定义一个组件类型
pub struct Name(pub String);

pub struct RootEntity (Id<Node>);

/// 测试组件脏
///迭代出脏的Position和对应的entity
pub fn query_tree(
	tree: EntityTree<Node>,
	root: Res<RootEntity>,
) {
	for k in tree.recursive_iter(root.0) {
		println!("modify entity_index: {:?}", k.offset());
	}
}

pub fn insert_tree(
	mut tree: EntityTreeMut<Node>,
	entitys: Query<Node, Id<Node>>
) {
	let mut i = 1;
	let mut es = Vec::new();
	es.push(Id::<Node>::null());

	for e in entitys.iter() {
		println!("parent: {:?}, {:?}, {}, i:{}", es[i >> 1].offset(), e.offset(), i >> 2, i);
		tree.insert_child(e, es[i >> 1], std::usize::MAX);
		es.push(e);
		i += 1;
	}
}


#[test]
fn test() {
	
	// 创建world
	let mut world = World::new();

	// 创建一个名为Node的原型，为该原型注册组件类型（一旦注册，不可修改）
	world.new_archetype::<Node>()
		.register::<Layer>()
		.register::<NodeUp<Node>>()
		.register::<NodeDown<Node>>()
		.create();

	let root = world.spawn::<Node>().id();
	world.insert_resource(RootEntity(root));

	let dispatcher = create_dispatcher(&mut world);

	for _e in 0..10 {
		world.spawn::<Node>();
	}

	// 创建一棵树
	let mut create_tree = insert_tree.system(&mut world);
	create_tree.run(());

	// 迭代树
	dispatcher.run();
}

fn create_dispatcher(world: &mut World) -> SingleDispatcher<MultiTaskRuntime> {
	let rt = AsyncRuntimeBuilder::default_multi_thread(
		None,
		None,
		None,
		None,
	);
	let iter_dirty_system = query_tree.system(world);

	let mut stage = StageBuilder::new();
	stage.add_node(iter_dirty_system);
	
	let mut stages = Vec::new();
	stages.push(Arc::new(stage.build(world)));
	let mut dispatcher = SingleDispatcher::new( rt);
	dispatcher.init(stages, world);

	return dispatcher;
}