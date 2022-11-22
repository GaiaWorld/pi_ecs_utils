//! OrDefault, 如果组件不存在，则取默认值

use std::marker::PhantomData;

use bevy_ecs::{query::{Access, FilteredAccess, WorldQuery, ReadFetch, ReadOnlyWorldQuery}, component::{StorageType, ComponentStorage, ComponentId}, archetype::{Archetype, ArchetypeComponentId}, world::{ World, FromWorld}, storage::Table, prelude::{Component, Entity}, system::Resource};
use derive_deref::{Deref, DerefMut};

/// 不存在T时，使用默认值。
/// 默认值取单例DefaultValue<T>
/// DefaultValue<T>默认为DefaultValue::from_world的返回值，也可被应用程序覆盖
pub struct OrDefault<T> {
	// value: &'w T,
	mark: PhantomData<T>,
}

unsafe impl<T: Component + FromWorld> ReadOnlyWorldQuery for OrDefault<T> {}

#[derive(Default, Debug, Deref, DerefMut, Resource)]
pub struct DefaultComponent<T>(pub T);

#[doc(hidden)]
pub struct OrDefaultState {
	component_state: ComponentId,
	default_res_component_id: ComponentId,
	default_res_archetype_component_id: ArchetypeComponentId,
}

/// SAFETY: access of `&T` is a subset of `Write<T>`
unsafe impl<T: Component + FromWorld> WorldQuery for OrDefault<T> {
    type Fetch<'w> = OrDefaultFetch<'w, T>;
    type Item<'w> = &'w T;
    type ReadOnly = Self;
    type State = OrDefaultState;

    fn shrink<'wlong: 'wshort, 'wshort>(item: &'wlong T) -> &'wshort T {
        item
    }

    const IS_DENSE: bool = {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => true,
            StorageType::SparseSet => false,
        }
    };

    const IS_ARCHETYPAL: bool = true;

    unsafe fn init_fetch<'w>(
        world: &'w World,
        state: &Self::State,
        last_change_tick: u32,
        change_tick: u32,
    ) -> OrDefaultFetch<'w, T> {
        OrDefaultFetch {
            inner: <&T>::init_fetch(world, &state.component_state, last_change_tick, change_tick),
			default_value: &world.get_resource_by_id(state.default_res_component_id).unwrap().deref::<DefaultComponent<T>>().0,
			matches: false,
        }
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        OrDefaultFetch {
            inner: <&T>::clone_fetch(&fetch.inner),
            default_value: fetch.default_value,
			matches: fetch.matches,
        }
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut Self::Fetch<'w>,
        state: &Self::State,
        archetype: &'w Archetype,
        table: &'w Table,
    ) {
		fetch.matches = <&T>::matches_component_set(&state.component_state, &|id| table.has_column(id));
		if fetch.matches {
			<&T>::set_archetype(&mut fetch.inner, &state.component_state, archetype, table)
		}
    }

    #[inline]
    unsafe fn set_table<'w>(
        fetch: &mut Self::Fetch<'w>,
        state: &Self::State,
        table: &'w Table,
    ) {
		fetch.matches = <&T>::matches_component_set(&state.component_state, &|id| table.has_column(id));
		if fetch.matches {
			<&T>::set_table(&mut fetch.inner, &state.component_state, table);
		}
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: usize,
    ) -> Self::Item<'w> {
		if fetch.matches {
			<&T>::fetch(&mut fetch.inner, entity, table_row)
		} else {
			fetch.default_value
		}
    }

    fn update_component_access(state: &Self::State, access: &mut FilteredAccess<ComponentId>) {
        <&T>::update_component_access(&state.component_state, access);
		assert!(
            !access.access().has_write(state.default_res_component_id),
            "&{} conflicts with a previous access in this query. Shared access cannot coincide with exclusive access.",
                std::any::type_name::<T>(),
        );
        access.add_read(state.default_res_component_id);
    }

    fn update_archetype_component_access(
        state: &Self::State,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
		<&T>::update_archetype_component_access(&state.component_state, archetype, access);
		access.add_read(state.default_res_archetype_component_id);
	}

    fn init_state(world: &mut World) -> OrDefaultState {

		if let None = world.get_resource::<DefaultComponent<T>>() {
			let v = DefaultComponent(T::from_world(world));
			world.insert_resource(v);
		}
		let default_res_component_id = world.components().get_resource_id(std::any::TypeId::of::<DefaultComponent<T>>()).unwrap();
		let default_res_archetype_component_id = world.storages().resources.get(default_res_component_id).unwrap().id();

		OrDefaultState {
			component_state: world.init_component::<T>(),
			default_res_component_id: default_res_component_id,
			default_res_archetype_component_id: default_res_archetype_component_id,
		}
    }

    fn matches_component_set(
        _state: &OrDefaultState,
        _set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        // set_contains_id(state)
		true
    }
}

/// The [`Fetch`] of `&mut T`.
#[doc(hidden)]
pub struct OrDefaultFetch<'w, T> {
	inner: ReadFetch<'w, T>,
	default_value: &'w T,
	matches: bool,
}


