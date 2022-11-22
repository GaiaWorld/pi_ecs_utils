//! 实体树

use std::mem::transmute;
use bevy_ecs::{prelude::{Entity, Component}, system::{Query, Commands, SystemParam}};
use derive_deref::{Deref, DerefMut};
use pi_null::Null;
use pi_slotmap_tree::{Up as Up1, Down as Down1, Storage, StorageMut, Tree, Layer as Layer1, ChildrenIterator as ChildrenIterator1, RecursiveIterator as RecursiveIterator1, InsertType};

// use pi_print_any::{println_any, out_any};

#[derive(Debug, Clone, Copy, Default, Component)]
pub struct Root;

#[derive(Debug, Clone, Deref, PartialEq, Eq, Copy)]
pub struct TreeKey(pub Entity);

const NULL_KEY: u64 = (1 as u64) << 62 + (1 as u64) << 32; // 为什么不能左移63位？？TODO
impl Null for TreeKey {
    fn null() -> Self {
		unsafe { transmute(NULL_KEY)}
    }

    fn is_null(&self) -> bool {
        let r = unsafe { transmute::<_, u64>(self.0)} == NULL_KEY;
		r
    }
}

#[derive(Debug, Clone, Default, Component)]
pub struct Layer(Layer1<TreeKey>);
impl Layer  {
	#[inline]
	pub fn layer(&self) -> usize{
		self.0.layer()
	}
	#[inline]
	pub fn root(&self) -> Entity {
		self.0.root().0
	}
}

#[derive(Debug, Clone, Default, Component)]
pub struct Up(Up1<TreeKey>);
impl Up  {
	#[inline]
	pub fn parent(&self) -> Entity {
		self.0.parent().0
	}
	#[inline]
	pub fn prev(&self) -> Entity {
		self.0.prev().0
	}
	#[inline]
	pub fn next(&self) -> Entity {
		self.0.next().0
	}
}

#[derive(Debug, Clone, Default, Component)]
pub struct Down(Down1<TreeKey>);
impl Down  {
	#[inline]
	pub fn head(&self) -> Entity {
		self.0.head().0
	}
	#[inline]
	pub fn tail(&self) -> Entity {
		self.0.tail().0
	}
	#[inline]
	pub fn len(&self) -> usize {
		self.0.len()
	}
	#[inline]
	pub fn count(&self) -> usize {
		self.0.count()
	}
}

// 放入EntityTree， 并为其实现一个Default方法
pub struct Down2(Down1<TreeKey>);
impl Default for Down2 {
    fn default() -> Self {
        Self(
			Down1 {
				head: TreeKey::null(),
				tail: TreeKey::null(),
				len: 0,
				count: 1,
			})
    }
}

#[derive(SystemParam)]
pub struct EntityTree<'w, 's> {
	layer_query: Query<'w, 's, &'static Layer>,
	up_query: Query<'w, 's, &'static Up>,
	down_query: Query<'w, 's, &'static Down>,
}

impl<'w, 's> Storage<TreeKey> for EntityTree<'w, 's> {
	fn get_up(&self, k: TreeKey) -> Option<&Up1<TreeKey>> {
		unsafe{transmute(self.get_up(k.0))}
	}
	fn up(&self, k: TreeKey) -> &Up1<TreeKey> {
		unsafe{transmute(self.up(k.0))}
	}

	fn get_layer(&self, k: TreeKey) -> Option<&Layer1<TreeKey>> {
		unsafe{transmute(self.get_layer(k.0))}
	}
	fn layer(&self, k: TreeKey) -> &Layer1<TreeKey> {
		unsafe{transmute(self.layer_query.get(k.0).unwrap())}
	}

	fn get_down(&self, k: TreeKey) -> Option<&Down1<TreeKey>> {
		unsafe{transmute(self.get_down(k.0))}
	}

	fn down(&self, k: TreeKey) -> &Down1<TreeKey> {
		unsafe{transmute(self.down(k.0))}
	}
}

impl<'w, 's> EntityTree<'w, 's> {
	pub fn get_up(&self, k: Entity) -> Option<&Up> {
		match self.up_query.get(k) {
			Ok(r) => Some(r),
			_ => None,
		}
	}
	pub fn up(&self, k: Entity) -> &Up {
		self.up_query.get(k).unwrap()
	}

	pub fn get_layer(&self, k: Entity) -> Option<&Layer> {
		match self.layer_query.get( k) {
			Ok(r) => Some(r),
			_ => None,
		}
	}
	pub fn layer(&self, k: Entity) -> &Layer{
		self.layer_query.get(k).unwrap()
	}

	pub fn get_down(&self, k: Entity) -> Option<&Down> {
		match self.down_query.get(k) {
			Ok(r) => Some(r),
			_ => None,
		}
	}

	pub fn down(&self, k: Entity) -> &Down {
		self.down_query.get(k).unwrap()
	}

	pub fn iter(&self, node_children_head: Entity) -> ChildrenIterator<EntityTree<'w, 's>> {
		ChildrenIterator {
			inner: ChildrenIterator1::new(self, TreeKey(node_children_head))
		}
	}

	/// 迭代指定节点的所有递归子元素
	pub fn recursive_iter(&self, node_children_head: Entity) -> RecursiveIterator<EntityTree<'w, 's>> {
		let head = TreeKey(node_children_head);
		let len = if head.is_null() {
			0
		} else {
			1
		};
		RecursiveIterator{inner:RecursiveIterator1::new(self, head, len)}
	}
}

pub struct ChildrenIterator<'a, S: Storage<TreeKey>> {
	inner: ChildrenIterator1<'a, TreeKey, S>
}

impl<'a, S: Storage<TreeKey>> Iterator for ChildrenIterator<'a, S> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
		unsafe{transmute(self.inner.next())}
    }
}

pub struct RecursiveIterator<'a, S: Storage<TreeKey>> {
	inner: RecursiveIterator1<'a, TreeKey, S>
}

impl<'a, S: Storage<TreeKey>> Iterator for RecursiveIterator<'a, S> {
    type Item = Entity;

    fn next(&mut self) -> Option<Self::Item> {
		unsafe{transmute(self.inner.next())}
    }
}


pub struct EntityTreeMut<'w, 's> {
	tree: Tree<TreeKey, TreeStorageMut<'w, 's>>,
}

impl<'w, 's> EntityTreeMut<'w, 's> {
	pub fn insert_child(&mut self, node: Entity, parent: Entity, index: usize) {
		self.tree.insert_child(TreeKey(node), TreeKey(parent), index);
	}

	pub fn insert_brother(&mut self, node: Entity, anchor: Entity, ty: InsertType) {
		self.tree.insert_brother(TreeKey(node), TreeKey(anchor), ty);
	}

	pub fn remove(&mut self, node: Entity) {
		self.tree.remove(TreeKey(node));
	}
}

#[derive(SystemParam)]
pub struct TreeStorageMut<'w, 's> {
	layer_query: Query<'w, 's, &'static mut Layer>,
	up_query: Query<'w, 's, &'static mut Up>,
	down_query: Query<'w, 's, &'static mut Down>,
	command: Commands<'w, 's>, // 用于插入Root组件
}

const _: () = {
    impl<'w, 's> bevy_ecs::system::SystemParam for EntityTreeMut<'w, 's> {
        type Fetch = FetchState<(
            <Query<'w, 's, &'static mut Layer> as bevy_ecs::system::SystemParam>::Fetch,
            <Query<'w, 's, &'static mut Up> as bevy_ecs::system::SystemParam>::Fetch,
            <Query<'w, 's, &'static mut Down> as bevy_ecs::system::SystemParam>::Fetch,
            <Commands<'w, 's> as bevy_ecs::system::SystemParam>::Fetch,
        )>;
    }
    #[doc(hidden)]
    pub struct FetchState<TSystemParamState> {
        state: TSystemParamState,
        marker: std::marker::PhantomData<fn() -> ()>,
    }
    unsafe impl<TSystemParamState: bevy_ecs::system::SystemParamState>
        bevy_ecs::system::SystemParamState for FetchState<TSystemParamState>
    {
        fn init(
            world: &mut bevy_ecs::world::World,
            system_meta: &mut bevy_ecs::system::SystemMeta,
        ) -> Self {
            Self {
                state: TSystemParamState::init(world, system_meta),
                marker: std::marker::PhantomData,
            }
        }
        fn new_archetype(
            &mut self,
            archetype: &bevy_ecs::archetype::Archetype,
            system_meta: &mut bevy_ecs::system::SystemMeta,
        ) {
            self.state.new_archetype(archetype, system_meta)
        }
        fn apply(&mut self, world: &mut bevy_ecs::world::World) {
            self.state.apply(world)
        }
    }
    impl<'w, 's> bevy_ecs::system::SystemParamFetch<'w, 's>
        for FetchState<(
            <Query<'w, 's, &'static mut Layer> as bevy_ecs::system::SystemParam>::Fetch,
            <Query<'w, 's, &'static mut Up> as bevy_ecs::system::SystemParam>::Fetch,
            <Query<'w, 's, &'static mut Down> as bevy_ecs::system::SystemParam>::Fetch,
            <Commands<'w, 's> as bevy_ecs::system::SystemParam>::Fetch,
        )>
    {
        type Item = EntityTreeMut<'w, 's>;
        unsafe fn get_param(
            state: &'s mut Self,
            system_meta: &bevy_ecs::system::SystemMeta,
            world: &'w bevy_ecs::world::World,
            change_tick: u32,
        ) -> Self::Item {
            EntityTreeMut{
				tree: Tree::new(TreeStorageMut { layer_query : < < Query < 'w , 's , & 'static mut Layer > as bevy_ecs :: system :: SystemParam > :: Fetch as bevy_ecs :: system :: SystemParamFetch > :: get_param (& mut state . state . 0 , system_meta , world , change_tick) , up_query : < < Query < 'w , 's , & 'static mut Up > as bevy_ecs :: system :: SystemParam > :: Fetch as bevy_ecs :: system :: SystemParamFetch > :: get_param (& mut state . state . 1 , system_meta , world , change_tick) , down_query : < < Query < 'w , 's , & 'static mut Down > as bevy_ecs :: system :: SystemParam > :: Fetch as bevy_ecs :: system :: SystemParamFetch > :: get_param (& mut state . state . 2 , system_meta , world , change_tick) , command : < < Commands < 'w , 's > as bevy_ecs :: system :: SystemParam > :: Fetch as bevy_ecs :: system :: SystemParamFetch > :: get_param (& mut state . state . 3 , system_meta , world , change_tick) , })
			}
        }
    }
    unsafe impl<
            TSystemParamState: bevy_ecs::system::SystemParamState + bevy_ecs::system::ReadOnlySystemParamFetch,
        > bevy_ecs::system::ReadOnlySystemParamFetch for FetchState<TSystemParamState>
    {
    }
};

impl<'w, 's> EntityTreeMut<'w, 's> {
	pub fn get_up(&self, k: Entity) -> Option<&Up> {
		unsafe{transmute(self.tree.get_up(TreeKey(k)))}
	}
	pub fn up(&self, k: Entity) -> &Up {
		unsafe{transmute(self.tree.up(TreeKey(k)))}
	}

	pub fn get_layer(&self, k: Entity) -> Option<&Layer> {
		unsafe{transmute(self.tree.get_layer(TreeKey(k)))}
	}
	pub fn layer(&self, k: Entity) -> &Layer{
		unsafe{transmute(self.tree.layer(TreeKey(k)))}
	}

	pub fn get_down(&self, k: Entity) -> Option<&Down> {
		unsafe{transmute(self.tree.get_down(TreeKey(k)))}
	}

	pub fn down(&self, k: Entity) -> &Down {
		unsafe{transmute(self.tree.down(TreeKey(k)))}
	}

	pub fn iter(&self, node_children_head: Entity) -> ChildrenIterator<TreeStorageMut<'w, 's>> {
		ChildrenIterator {
			inner: self.tree.iter(TreeKey(node_children_head))
		}
	}

	/// 迭代指定节点的所有递归子元素
	pub fn recursive_iter(&self, node_children_head: Entity) -> RecursiveIterator<TreeStorageMut<'w, 's>> {
		RecursiveIterator{inner:self.tree.recursive_iter(TreeKey(node_children_head))}
	}
}

impl<'w, 's> Storage<TreeKey> for TreeStorageMut<'w, 's> {
	fn get_up(&self, k: TreeKey) -> Option<&Up1<TreeKey>> {
		unsafe{transmute(match self.up_query.get(k.0) {
			Ok(r) => Some(r),
			_ => None,
		})}
	}
	fn up(&self, k: TreeKey) -> &Up1<TreeKey> {
		unsafe{transmute(self.up_query.get(k.0).unwrap())}
	}

	fn get_layer(&self, k: TreeKey) -> Option<&Layer1<TreeKey>> {
		unsafe{transmute(match self.layer_query.get( k.0) {
			Ok(r) => Some(r),
			_ => None,
		})}
	}
	fn layer(&self, k: TreeKey) -> &Layer1<TreeKey> {
		unsafe{transmute(self.layer_query.get(k.0).unwrap())}
	}

	fn get_down(&self, k: TreeKey) -> Option<&Down1<TreeKey>> {
		unsafe{transmute(match self.down_query.get(k.0) {
			Ok(r) => Some(r),
			_ => None,
		})}
	}
	fn down(&self, k: TreeKey) -> &Down1<TreeKey> {
		unsafe{transmute(self.down_query.get(k.0).unwrap())}
	}
}

impl<'w, 's> StorageMut<TreeKey> for TreeStorageMut<'w, 's> {
	fn get_up_mut(&mut self, k: TreeKey) -> Option<&mut Up1<TreeKey>> {
		unsafe{transmute(match self.up_query.get_mut(k.0) {
			Ok(r) => Some(r.into_inner()),
			_ => None,
		})}
	}
	fn up_mut(&mut self, k: TreeKey) -> &mut Up1<TreeKey> {
		unsafe{transmute(&mut self.up_query.get_mut(k.0).unwrap())}
	}

	fn set_up(&mut self, k: TreeKey, up: Up1<TreeKey>) {
		unsafe{transmute(if let Ok(mut write) = self.up_query.get_mut(k.0) {
			*write = Up(up);
		})}
	}

	fn remove_up(&mut self, k: TreeKey) {
		unsafe{transmute(if let Ok(mut write) = self.up_query.get_mut(k.0) {
			*write = Up(Up1::default());
		})}
	}

	fn set_layer(&mut self, k: TreeKey, layer: Layer1<TreeKey>) {
		unsafe{transmute(if let Ok(mut write) = self.layer_query.get_mut(k.0) {
			*write = Layer(layer);
		})}
	}
	
	fn remove_layer(&mut self, k: TreeKey) {
		unsafe{transmute(if let Ok(mut write) = self.layer_query.get_mut(k.0) {
			*write = Layer(Layer1::default());
		})}
	}

	fn get_down_mut(&mut self, k: TreeKey) -> Option<&mut Down1<TreeKey>> {
		unsafe{transmute(match self.down_query.get_mut(k.0) {
			Ok(r) => Some(r.into_inner()),
			_ => None,
		})}
	}

	fn down_mut(&mut self, k: TreeKey) -> &mut Down1<TreeKey> {
		unsafe{transmute(&mut self.down_query.get_mut(k.0).unwrap())}
	}

	fn set_down(&mut self, k: TreeKey, down: Down1<TreeKey>) {
		unsafe{transmute(if let Ok(mut write) = self.down_query.get_mut(k.0) {
			*write = Down(down);
		})}
	}

	fn remove_down(&mut self, k: TreeKey) {
		unsafe{transmute(if let Ok(mut write) = self.down_query.get_mut(k.0) {
			*write = Down(Down1::default());
		})}
	}

	// 通知， TODO
	fn set_root(&mut self, k: TreeKey) {
		self.command.entity(k.0).insert(Root);
	}

	fn remove_root(&mut self, k: TreeKey) {
		self.command.entity(k.0).remove::<Root>();
	}
}

// // #[derive(Deref)]
// // pub struct EntityTree<'s, A: ArchetypeIdent>(Tree<Id, &'s IdtreeState>);

// // // impl<A: ArchetypeIdent> Clone for EntityTree {
// // // 	fn clone(&self) -> Self {
// // // 		Self(Tree::new(IdtreeState(self.0.get_storage().0.clone())))
// // // 	}
// // // }

