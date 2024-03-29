use std::mem::transmute;

use derive_deref::{Deref, DerefMut};
use pi_ecs::{entity::Id, archetype::ArchetypeIdent, prelude::{QueryState, World, Write, SystemParamState, SystemState, SystemParamFetch, SystemParam}};
use pi_slotmap_tree::{Up as Up1, Down as Down1, Storage, StorageMut, Tree, Layer as Layer1};
// use pi_print_any::{println_any, out_any};

#[derive(Debug, Clone, Copy, Default)]
pub struct Root;
pub type Layer<T> = Layer1<Id<T>>;

pub type Up<T> = Up1<Id<T>>;

pub type Down<T> = Down1<Id<T>>;

pub struct TreeStorage<A: ArchetypeIdent> {
	layer_query: QueryState<A, &'static Layer<A>>,
	up_query: QueryState<A, &'static Up<A>>,
	down_query: QueryState<A, &'static Down<A>>,
	world: World,
}

impl<A: ArchetypeIdent> Storage<Id<A>> for &IdtreeState<A> {
	fn get_up(&self, k: Id<A>) -> Option<&Up<A>> {
		self.0.up_query.get(&self.0.world, k)
	}
	fn up(&self, k: Id<A>) -> &Up<A> {
		self.0.up_query.get(&self.0.world, k).unwrap()
	}

	fn get_layer(&self, k: Id<A>) -> Option<&Layer<A>> {
		unsafe { std::mem::transmute(self.0.layer_query.get(&self.0.world,k)) }
	}
	fn layer(&self, k: Id<A>) -> &Layer<A> {
		self.0.layer_query.get(&self.0.world,k).unwrap()
	}

	fn get_down(&self, k: Id<A>) -> Option<&Down<A>> {
		self.0.down_query.get(&self.0.world,k)
	}
	fn down(&self, k: Id<A>) -> &Down<A> {
		self.0.down_query.get(&self.0.world,k).unwrap()
	}
}

pub struct TreeStorageMut<A: ArchetypeIdent> {
	layer_query: QueryState<A, Write<Layer<A>>>,
	up_query: QueryState<A, Write<Up<A>>>,
	down_query: QueryState<A, Write<Down<A>>>,
	root_query:  QueryState<A, Write<Root>>,
	// common_layer: Commands<Layer>,
	// common_up: Commands<NodeUp<A>>,
	// common_down: Commands<NodeDown<A>>,
	world: World,
	last_change_tick: u32,
    change_tick: u32,
}

impl<A: ArchetypeIdent> Storage<Id<A>> for &mut IdtreeMutState<A> {
	fn get_up(&self, k: Id<A>) -> Option<&Up<A>> {
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
	fn up(&self, k: Id<A>) -> &Up<A> {
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

	fn get_layer(&self, k: Id<A>) -> Option<&Layer<A>> {
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

	fn layer(&self, k: Id<A>) -> &Layer<A> {
		unsafe { transmute(self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		).unwrap().get().unwrap()) }
	}

	fn get_down(&self, k: Id<A>) -> Option<&Down<A>> {
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
	fn down(&self, k: Id<A>) -> &Down<A> {
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

impl<A: ArchetypeIdent> StorageMut<Id<A>> for &mut IdtreeMutState<A> {
	fn get_up_mut(&mut self, k: Id<A>) -> Option<&mut Up<A>> {
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
	fn up_mut(&mut self, k: Id<A>) -> &mut Up<A> {
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

	fn set_up(&mut self, k: Id<A>, up: Up<A>) {
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

	fn set_layer(&mut self, k: Id<A>, layer: Layer<A>) {
		if let Some(mut write_item) = unsafe { self.0.layer_query.get_unchecked_manual(
			&self.0.world, 
			k, 
			self.0.last_change_tick, 
			self.0.change_tick
		)} {
			write_item.write(layer)
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

	fn get_down_mut(&mut self, k: Id<A>) -> Option<&mut Down<A>> {
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

	fn down_mut(&mut self, k: Id<A>) -> &mut Down<A> {
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

	fn set_down(&mut self, k: Id<A>, down: Down<A>) {
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

pub struct IdtreeState<A: ArchetypeIdent>(TreeStorage<A>);

unsafe impl<A: ArchetypeIdent> SystemParamState for IdtreeState<A> {
    type Config = ();
	
	/// 检查数据访问冲突
	/// 一些状态的初始化
	/// 添加监听器监听数据的改变，进行脏设置
    fn init(world: &mut World, system_state: &mut SystemState, _config: Self::Config) -> Self {
		let (_last_change_tick, _change_tick) = (world.last_change_tick(), world.change_tick());

		let layer_query: QueryState<A, &'static Layer<A>, ()> = SystemParamState::init(world, system_state, ());
		let up_query: QueryState<A, &'static Up<A>, ()> = SystemParamState::init(world, system_state, ());
		let down_query: QueryState<A, &'static Down<A>, ()> = SystemParamState::init(world, system_state, ());

		IdtreeState(
			TreeStorage {
				layer_query,
				up_query,
				down_query,
				world: world.clone()
			}
		)
    }

	fn apply(&mut self, _world: &mut World) {}

    fn default_config() {}
}

impl<'w, 's, A: ArchetypeIdent> SystemParamFetch<'w, 's> for IdtreeState<A> {
    type Item = EntityTree<'s, A>;

    #[inline]
    unsafe fn get_param(
        state: &'s mut Self,
        system_state: &SystemState,
        world: &'w World,
        change_tick: u32,
    ) -> Self::Item {
		state.0.up_query.setting(world, system_state.last_change_tick, change_tick);
		state.0.down_query.setting(world, system_state.last_change_tick, change_tick);
		state.0.layer_query.setting(world, system_state.last_change_tick, change_tick);
		EntityTree(Tree::new(state))
    }
}

pub struct IdtreeMutState<A: ArchetypeIdent>(TreeStorageMut<A>);

unsafe impl<A: ArchetypeIdent> SystemParamState for IdtreeMutState<A> {
    type Config = ();
	
	/// 检查数据访问冲突
	/// 一些状态的初始化
	/// 添加监听器监听数据的改变，进行脏设置
    fn init(world: &mut World, system_state: &mut SystemState, _config: Self::Config) -> Self {
		let (last_change_tick, change_tick) = (world.last_change_tick(), world.change_tick());

		let layer_query: QueryState<A, Write<Layer<A>>, ()> = SystemParamState::init(world, system_state, ());
		let up_query: QueryState<A, Write<Up<A>>, ()> = SystemParamState::init(world, system_state, ());

		let down_query: QueryState<A, Write<Down<A>>, ()> = SystemParamState::init(world, system_state, ());

		let root_query: QueryState<A, Write<Root>, ()> = SystemParamState::init(world, system_state, ());

		IdtreeMutState(
			TreeStorageMut {
				layer_query,
				up_query,
				down_query,
				root_query,
				world: world.clone(),
				last_change_tick,
				change_tick
			}
		)
    }

	fn apply(&mut self, _world: &mut World) {}

    fn default_config() {}
}

impl<'w, 's, A: ArchetypeIdent> SystemParamFetch<'w, 's> for IdtreeMutState<A> {
    type Item = EntityTreeMut<'s, A>;

    #[inline]
    unsafe fn get_param(
        state: &'s mut Self,
        system_state: &SystemState,
        world: &'w World,
        change_tick: u32,
    ) -> Self::Item {
		state.0.up_query.setting(world, system_state.last_change_tick, change_tick);
		state.0.down_query.setting(world, system_state.last_change_tick, change_tick);
		state.0.layer_query.setting(world, system_state.last_change_tick, change_tick);
		EntityTreeMut(Tree::new(state))
    }
}

#[derive(Deref)]
pub struct EntityTree<'s, A: ArchetypeIdent>(Tree<Id<A>, &'s IdtreeState<A>>);

// impl<A: ArchetypeIdent> Clone for EntityTree<A> {
// 	fn clone(&self) -> Self {
// 		Self(Tree::new(IdtreeState(self.0.get_storage().0.clone())))
// 	}
// }

#[derive(Deref, DerefMut)]
pub struct EntityTreeMut<'s, A: ArchetypeIdent>(Tree<Id<A>, &'s mut IdtreeMutState<A>>);

// impl<A: ArchetypeIdent> Clone for EntityTreeMut<A> {
// 	fn clone(&self) -> Self {
// 		Self(Tree::new(IdtreeMutState(self.0.get_storage().0.clone())))
// 	}
// }


impl<'s, A: ArchetypeIdent> SystemParam for EntityTree<'s, A> {
    type Fetch = IdtreeState<A>;
}

impl<'s, A: ArchetypeIdent> SystemParam for EntityTreeMut<'s, A> {
    type Fetch = IdtreeMutState<A>;
}
