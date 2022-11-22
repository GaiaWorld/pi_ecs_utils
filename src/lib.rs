#![feature(prelude_import)]
#![feature(min_specialization)]
#![feature(associated_type_bounds)]
#![feature(associated_type_defaults)]
#![feature(return_position_impl_trait_in_trait)]

// #[macro_use]
// extern crate serde;
// #[macro_use]
// extern crate derive_deref;
// #[macro_use]
// extern crate pi_ecs_macros;

pub mod system_param;
pub mod query;
pub mod async_system;
// pub mod dispatch;


pub mod prelude {
    pub use crate::{
        system_param::{
			tree::{Layer, Down, Up, EntityTreeMut, EntityTree, Root},
			layer_dirty::LayerDirty
		},
		query::or_default::{OrDefault, DefaultComponent},
		
    };
}

