use std::sync::Arc;

use derive_deref::{Deref, DerefMut};
use pi_ecs::{entity::Entity, archetype::ArchetypeIdent, prelude::{QueryState, World, Write, SystemParamState, SystemState, SystemParamFetch, SystemParam}};
use pi_slotmap_tree::{Up, Down, Storage, StorageMut, Tree};

#[derive(Debug, Clone, Copy, Deref, Default)]
pub struct Layer(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct Root;

pub type NodeUp = Up<Entity>;

pub type NodeDown = Down<Entity>;

pub struct TreeStorage<A: ArchetypeIdent> {
	layer_query: QueryState<A, &'static Layer>,
	up_query: QueryState<A, &'static NodeUp>,
	down_query: QueryState<A, &'static NodeDown>,
	world: World,
}

impl<A: ArchetypeIdent> Storage<Entity> for IdtreeState<A> {
	fn get_up(&self, k: Entity) -> Option<&NodeUp> {
		self.0.up_query.get(&self.0.world, k)
	}
	fn up(&self, k: Entity) -> &NodeUp {
		self.0.up_query.get(&self.0.world, k).unwrap()
	}

	fn get_layer(&self, k: Entity) -> Option<&usize> {
		unsafe { std::mem::transmute(self.0.layer_query.get(&self.0.world,k)) }
	}
	fn layer(&self, k: Entity) -> usize {
		self.0.layer_query.get(&self.0.world,k).unwrap().0
	}

	fn get_down(&self, k: Entity) -> Option<&NodeDown> {
		self.0.down_query.get(&self.0.world,k)
	}
	fn down(&self, k: Entity) -> &NodeDown {
		self.0.down_query.get(&self.0.world,k).unwrap()
	}
}

struct TreeStorageMut<A: ArchetypeIdent> {
	layer_query: QueryState<A, Write<Layer>>,
	up_query: QueryState<A, Write<NodeUp>>,
	down_query: QueryState<A, Write<NodeDown>>,
	root_query:  QueryState<A, Write<Root>>,
	// common_layer: Commands<Layer>,
	// common_up: Commands<NodeUp>,
	// common_down: Commands<NodeDown>,
	world: World,
	last_change_tick: u32,
    change_tick: u32,
}

impl<A: ArchetypeIdent> Storage<Entity> for IdtreeMutState<A> {
	fn get_up(&self, k: Entity) -> Option<&NodeUp> {
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
	fn up(&self, k: Entity) -> &NodeUp {
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

	fn get_layer(&self, k: Entity) -> Option<&usize> {
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

	fn layer(&self, k: Entity) -> usize {
		**unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)}.unwrap().get().unwrap()
	}

	fn get_down(&self, k: Entity) -> Option<&NodeDown> {
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
	fn down(&self, k: Entity) -> &NodeDown {
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

impl<A: ArchetypeIdent> StorageMut<Entity> for IdtreeMutState<A> {
	fn get_up_mut(&mut self, k: Entity) -> Option<&mut NodeUp> {
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
	fn up_mut(&mut self, k: Entity) -> &mut NodeUp {
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

	fn set_up(&mut self, k: Entity, up: Up<Entity>) {
		if let Some(mut write_item) = unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			let parent = up.parent();
			write_item.write(up);
			

			let parent_layer = unsafe { self.0.layer_query.get_unchecked(
				&self.0.world, 
				parent
			)};
			// parent layer存在
			if parent_layer.get().is_some() {
				write_item.notify_modify();
			}
		}
	}

	fn remove_up(&mut self, k: Entity) {
		if let Some(mut write_item) = unsafe { self.0.up_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn set_layer(&mut self, k: Entity, layer: usize) {
		if let Some(mut write_item) = unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(Layer(layer))
		}
	}
	
	fn remove_layer(&mut self, k: Entity) {
		if let Some(mut write_item) = unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn get_down_mut(&mut self, k: Entity) -> Option<&mut NodeDown> {
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

	fn down_mut(&mut self, k: Entity) -> &mut NodeDown {
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

	fn set_down(&mut self, k: Entity, down: NodeDown) {
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

	fn remove_down(&mut self, k: Entity) {
		if let Some(mut write_item) = unsafe { self.0.down_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.remove();
		}
	}

	fn set_root(&mut self, k: Entity) {
		if let Some(mut write_item) = unsafe { self.0.root_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(Root);
		}
	}
	fn remove_root(&mut self, k: Entity) {
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

		let up_state: Arc<QueryState<A, &'static NodeUp, ()>> = SystemParamState::init(world, system_state, ());
		let up_query = match Arc::try_unwrap(up_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let down_state: Arc<QueryState<A, &'static NodeDown, ()>> = SystemParamState::init(world, system_state, ());
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

		let up_state: Arc<QueryState<A, Write<NodeUp>, ()>> = SystemParamState::init(world, system_state, ());
		let up_query = match Arc::try_unwrap(up_state) {
			Ok(r) => r,
			_ => panic!("-----------")
		};

		let down_state: Arc<QueryState<A, Write<NodeDown>, ()>> = SystemParamState::init(world, system_state, ());

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
pub struct EntityTree<A: ArchetypeIdent>(Tree<Entity, IdtreeState<A>>);

impl<A: ArchetypeIdent> Clone for EntityTree<A> {
	fn clone(&self) -> Self {
		Self(Tree::new(IdtreeState(self.0.get_storage().0.clone())))
	}
}

#[derive(Deref, DerefMut)]
pub struct EntityTreeMut<A: ArchetypeIdent>(Tree<Entity,IdtreeMutState<A>>);

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