use std::sync::Arc;

use derive_deref::{Deref, DerefMut};
use pi_ecs::{entity::Id, archetype::ArchetypeIdent, prelude::{QueryState, World, Write, SystemParamState, SystemState, SystemParamFetch, SystemParam}};
use pi_slotmap_tree::{Up, Down, Storage, StorageMut, Tree};

#[derive(Debug, Clone, Copy, Deref, Default)]
pub struct Layer(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct Root;

pub type NodeUp<T> = Up<Id<T>>;

pub type NodeDown<T> = Down<Id<T>>;

pub struct TreeStorage<A: ArchetypeIdent> {
	layer_query: QueryState<A, &'static Layer>,
	up_query: QueryState<A, &'static NodeUp<A>>,
	down_query: QueryState<A, &'static NodeDown<A>>,
	world: World,
}

impl<A: ArchetypeIdent> Storage<Id<A>> for IdtreeState<A> {
	fn get_up(&self, k: Id<A>) -> Option<&NodeUp<A>> {
		self.0.up_query.get(&self.0.world, k)
	}
	fn up(&self, k: Id<A>) -> &NodeUp<A> {
		self.0.up_query.get(&self.0.world, k).unwrap()
	}

	fn get_layer(&self, k: Id<A>) -> Option<&usize> {
		unsafe { std::mem::transmute(self.0.layer_query.get(&self.0.world,k)) }
	}
	fn layer(&self, k: Id<A>) -> usize {
		self.0.layer_query.get(&self.0.world,k).unwrap().0
	}

	fn get_down(&self, k: Id<A>) -> Option<&NodeDown<A>> {
		self.0.down_query.get(&self.0.world,k)
	}
	fn down(&self, k: Id<A>) -> &NodeDown<A> {
		self.0.down_query.get(&self.0.world,k).unwrap()
	}
}

struct TreeStorageMut<A: ArchetypeIdent> {
	layer_query: QueryState<A, Write<Layer>>,
	up_query: QueryState<A, Write<NodeUp<A>>>,
	down_query: QueryState<A, Write<NodeDown<A>>>,
	root_query:  QueryState<A, Write<Root>>,
	// common_layer: Commands<Layer>,
	// common_up: Commands<NodeUp<A>>,
	// common_down: Commands<NodeDown<A>>,
	world: World,
	last_change_tick: u32,
    change_tick: u32,
}

impl<A: ArchetypeIdent> Storage<Id<A>> for IdtreeMutState<A> {
	fn get_up(&self, k: Id<A>) -> Option<&NodeUp<A>> {
		match unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			Some(a) => unsafe {
				std::mem::transmute(a.get()) 
			},
			None => None,
		}
	}
	fn up(&self, k: Id<A>) -> &NodeUp<A> {
		let r = unsafe {self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		).unwrap()};
		
		unsafe {
			std::mem::transmute(r.get().unwrap())
		}
	}

	fn get_layer(&self, k: Id<A>) -> Option<&usize> {
		match unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			Some(a) => unsafe { std::mem::transmute(a.get()) },
			None => None,
		}
	}

	fn layer(&self, k: Id<A>) -> usize {
		**unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)}.unwrap().get().unwrap()
	}

	fn get_down(&self, k: Id<A>) -> Option<&NodeDown<A>> {
		match unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			Some(a) => unsafe {
				std::mem::transmute(a.get())
			},
			None => None,
		}
	}
	fn down(&self, k: Id<A>) -> &NodeDown<A> {
		let r = unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)}.unwrap();
		unsafe {
			std::mem::transmute(r.get().unwrap())
		}
	}
}

impl<A: ArchetypeIdent> StorageMut<Id<A>> for IdtreeMutState<A> {
	fn get_up_mut(&mut self, k: Id<A>) -> Option<&mut NodeUp<A>> {
		let r = match unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			Some(mut a) => unsafe {
				std::mem::transmute(a.get_mut())
			},
			None => None,
		};
		r
	}
	fn up_mut(&mut self, k: Id<A>) -> &mut NodeUp<A> {
		let mut r = unsafe {self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		).unwrap()};
		
		unsafe {
			std::mem::transmute(r.get_mut().unwrap())
		}
	}

	fn set_up(&mut self, k: Id<A>, up: Up<Id<A>>) {
		if let Some(mut write_item) = unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(up);
		}
	}

	fn remove_up(&mut self, k: Id<A>) {
		if let Some(mut write_item) = unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn set_layer(&mut self, k: Id<A>, layer: usize) {
		if let Some(mut write_item) = unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(Layer(layer))
		}
	}
	
	fn remove_layer(&mut self, k: Id<A>) {
		if let Some(mut write_item) = unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn get_down_mut(&mut self, k: Id<A>) -> Option<&mut NodeDown<A>> {
		let r = match unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			Some(mut a) => unsafe {
				std::mem::transmute(a.get_mut())
			},
			None => None,
		};
		r
	}

	fn down_mut(&mut self, k: Id<A>) -> &mut NodeDown<A> {
		let mut r = unsafe {self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		).unwrap()};
		
		unsafe {
			std::mem::transmute(r.get_mut().unwrap())
		}
	}

	fn set_down(&mut self, k: Id<A>, down: NodeDown<A>) {
		if let Some(mut write_item) = unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			unsafe {
				std::mem::transmute(write_item.write(down))
			}
		}
	}

	fn remove_down(&mut self, k: Id<A>) {
		if let Some(mut write_item) = unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn set_root(&mut self, k: Id<A>) {
		if let Some(mut write_item) = unsafe { self.0.root_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(Root);
		}
	}
	fn remove_root(&mut self, k: Id<A>) {
		if let Some(mut write_item) = unsafe { self.0.root_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}
}

#[derive(Clone)]
pub struct IdtreeState<A: ArchetypeIdent>(Arc<TreeStorage<A>>);

unsafe impl<A: ArchetypeIdent> SystemParamState for IdtreeState<A> {
    type Config = ();
	
	/// 检查数据访问冲突
	/// 一些状态的初始化
	/// 添加监听器监听数据的改变，进行脏设置
    fn init(world: &mut World, system_state: &mut SystemState, _config: Self::Config) -> Self {
		let (_last_change_tick, _change_tick) = (world.last_change_tick(), world.change_tick());

		let layer_state: Arc<QueryState<A, &'static Layer, ()>> = SystemParamState::init(world, system_state, ());
		let layer_query = match Arc::try_unwrap(layer_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let up_state: Arc<QueryState<A, &'static NodeUp<A>, ()>> = SystemParamState::init(world, system_state, ());
		let up_query = match Arc::try_unwrap(up_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let down_state: Arc<QueryState<A, &'static NodeDown<A>, ()>> = SystemParamState::init(world, system_state, ());
		let down_query = match Arc::try_unwrap(down_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		IdtreeState(Arc::new(
			TreeStorage {
				layer_query,
				up_query,
				down_query,
				world: world.clone()
			}
		))
    }

	fn apply(&mut self, _world: &mut World) {}

    fn default_config() {}
}

impl<'w, 's, A: ArchetypeIdent> SystemParamFetch<'w, 's> for IdtreeState<A> {
    type Item = EntityTree<A>;

    #[inline]
    unsafe fn get_param(
        state: &'s mut Self,
        _system_state: &SystemState,
        _world: &'w World,
        _change_tick: u32,
    ) -> Self::Item {
		EntityTree(Tree::new(IdtreeState(state.0.clone())))
    }
}

pub struct IdtreeMutState<A: ArchetypeIdent>(Arc<TreeStorageMut<A>>);

unsafe impl<A: ArchetypeIdent> SystemParamState for IdtreeMutState<A> {
    type Config = ();
	
	/// 检查数据访问冲突
	/// 一些状态的初始化
	/// 添加监听器监听数据的改变，进行脏设置
    fn init(world: &mut World, system_state: &mut SystemState, _config: Self::Config) -> Self {
		let (last_change_tick, change_tick) = (world.last_change_tick(), world.change_tick());

		let layer_state: Arc<QueryState<A, Write<Layer>, ()>> = SystemParamState::init(world, system_state, ());
		let layer_query = match Arc::try_unwrap(layer_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let up_state: Arc<QueryState<A, Write<NodeUp<A>>, ()>> = SystemParamState::init(world, system_state, ());
		let up_query = match Arc::try_unwrap(up_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let down_state: Arc<QueryState<A, Write<NodeDown<A>>, ()>> = SystemParamState::init(world, system_state, ());

		let root_state: Arc<QueryState<A, Write<Root>, ()>> = SystemParamState::init(world, system_state, ());

		let down_query = match Arc::try_unwrap(down_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};
		let root_query = match Arc::try_unwrap(root_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		IdtreeMutState(Arc::new(
			TreeStorageMut {
				layer_query,
				up_query,
				down_query,
				root_query,
				world: world.clone(),
				last_change_tick,
				change_tick
			}
		))
    }

	fn apply(&mut self, _world: &mut World) {}

    fn default_config() {}
}

impl<'w, 's, A: ArchetypeIdent> SystemParamFetch<'w, 's> for IdtreeMutState<A> {
    type Item = EntityTreeMut<A>;

    #[inline]
    unsafe fn get_param(
        state: &'s mut Self,
        _system_state: &SystemState,
        _world: &'w World,
        _change_tick: u32,
    ) -> Self::Item {
		EntityTreeMut(Tree::new(IdtreeMutState(state.0.clone())))
    }
}

#[derive(Deref)]
pub struct EntityTree<A: ArchetypeIdent>(Tree<Id<A>, IdtreeState<A>>);

impl<A: ArchetypeIdent> Clone for EntityTree<A> {
	fn clone(&self) -> Self {
		Self(Tree::new(IdtreeState(self.0.get_storage().0.clone())))
	}
}

#[derive(Deref, DerefMut)]
pub struct EntityTreeMut<A: ArchetypeIdent>(Tree<Id<A>,IdtreeMutState<A>>);

impl<A: ArchetypeIdent> Clone for EntityTreeMut<A> {
	fn clone(&self) -> Self {
		Self(Tree::new(IdtreeMutState(self.0.get_storage().0.clone())))
	}
}


impl<A: ArchetypeIdent> SystemParam for EntityTree<A> {
    type Fetch = IdtreeState<A>;
}

impl<A: ArchetypeIdent> SystemParam for EntityTreeMut<A> {
    type Fetch = IdtreeMutState<A>;
}