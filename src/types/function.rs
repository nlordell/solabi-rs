//! Solidity ABI function pointer type.

use super::{Primitive, Word};
use ethaddr::Address;

/// A function selector type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Selector(pub [u8; 4]);

impl AsRef<[u8]> for Selector {
    fn as_ref(&self) -> &[u8] {
        &self.0[..]
    }
}

/// Recent Solidity ABI version support function pointers. They are encoded as
/// a contract address and a selector packed into a `bytes24`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct FunctionPtr {
    /// The contract address that will be used when calling this function
    /// pointer.
    pub address: Address,
    /// The selector of the contract function getting called.
    pub selector: Selector,
}

impl Primitive for FunctionPtr {
    fn to_word(&self) -> Word {
        let mut word = Word::default();
        word[..20].copy_from_slice(self.address.as_ref());
        word[20..24].copy_from_slice(self.selector.as_ref());
        word
    }
}
