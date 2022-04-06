/// 测试系统参数: LayerDirty
/// 该参数监听组件的变化，根据其实体在Tree上的层次，来记录层次脏
/// 该参数提供iter方法，用于迭代已经脏的实体
/// LayerDirty<A, F, N=()>, A为原型类型、F为过滤器、N默认为(),其是Tree<K, N>上的一个泛型

use std::sync::Arc;

use pi_ecs::{prelude::{Query, IntoSystem, StageBuilder, SingleDispatcher, Dispatcher, Write, System}, entity::Entity, world::World, storage::Offset};
use pi_ecs::query::filter_change::Changed;
use pi_ecs_utils::prelude::{LayerDirty, NodeUp, NodeDown, Layer, EntityTreeMut};
use pi_async::rt::{AsyncRuntime, multi_thread::{MultiTaskRuntimeBuilder, StealableTaskPool}};
use pi_null::Null;

#[derive(Default)]
pub struct Node;

#[derive(Debug, Default)]
/// 定义一个组件类型
pub struct Name(pub String);


/// 测试组件脏
///迭代出脏的Position和对应的entity
pub fn iter_dirty(
	q: Query<Node, (Entity, &Name)>,
	dirtys: LayerDirty<Node, Changed<Name>>, // 该声明隐意：world上必须存在Tree<LocalVersion, ()>资源
) {
	for k in dirtys.iter() {
		let position = q.get(k.clone());
		println!("modify entity_index: {:?}, name:{:?}", k.local().offset(), position);
	}
}

pub struct RootEntity (Entity);

pub fn create_tree_sys(
	mut tree: EntityTreeMut<Node>,
	entitys: Query<Node, Entity>,
	mut names: Query<Node, Write<Name>>,
) {
	let mut i = 1;
	let mut es = Vec::new();
	es.push(Entity::null());

	for e in entitys.iter() {
		println!("parent: {:?}, {:?}, {}, i:{}", es[i >> 1].local().offset(), e.local().offset(), i >> 2, i);
		tree.insert_child(e, es[i >> 1], std::usize::MAX);
		names.get_mut(e).unwrap().write(Name(format!("{}-{}", i >> 1, i - (i >> 1 << 1))));
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
		.register::<Name>()
		.register::<Layer>()
		.register::<NodeUp>()
		.register::<NodeDown>()
		.create();

	let dispatcher = create_dispatcher(&mut world);

	let mut entitys = Vec::new();
	// 创建原型为Node的实体，并为该实体添加组件（必须是在Node中注册过的组件， 否则无法插入）
	let name = format!("{}", 0);
	let root = world.spawn::<Node>()
		.insert(Name(name.clone()))
		.id();
	entitys.push(root.clone());

	world.insert_resource(RootEntity(root));

	for _e in 0..10 {
		entitys.push(world.spawn::<Node>().id());
	}

	let mut create_tree_s = create_tree_sys.system(&mut world);
	create_tree_s.run(());

	println!("change all(0 and children): ");
	dispatcher.run();
	std::thread::sleep(std::time::Duration::new(1, 0));

	world.insert_component(entitys[4].clone(), Name("2-1-0".to_string()));
	println!("change 00 and children: ");
	dispatcher.run();

	std::thread::sleep(std::time::Duration::new(2, 0));
}

fn create_dispatcher(world: &mut World) -> SingleDispatcher<StealableTaskPool<()>> {
	let rt = AsyncRuntime::Multi(MultiTaskRuntimeBuilder::default().build());
	let iter_dirty_system = iter_dirty.system(world);

	let mut stage = StageBuilder::new();
	stage.add_node(iter_dirty_system);
	
	let mut stages = Vec::new();
	stages.push(Arc::new(stage.build()));
	let dispatcher = SingleDispatcher::new(stages, world, rt);

	return dispatcher;
}