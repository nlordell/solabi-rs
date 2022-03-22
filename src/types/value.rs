//! Module containing dynamic Solidity value.

use crate::encode::{Encode, Encoder, Size};

/// A Solidity value.
///
/// This type is capabable of represenging all Solidity values dynamically,
/// allowing for "dynamic" encoding and decoding. This is analogous to
/// `serde_json::Value` for Solidity values.
pub enum Value {}

impl Encode for Value {
    fn size(&self) -> Size {
        todo!()
    }

    fn encode(&self, _: &mut Encoder) {
        todo!()
    }
}
