//! Solidity primitive integer types.

use ethnum::{I256, U256};

/// An signed integer with the specified width.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Int<const N: usize>(pub I256);

/// An unsigned integer with the specified width.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Uint<const N: usize>(pub U256);
