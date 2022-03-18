//! Solidity ABI function pointer type.

use crate::types::address::Address;

/// A function selector type.
pub struct Selector(pub [u8; 4]);

/// Recent Solidity ABI version support function pointers. They are encoded as
/// a contract address and a selector packed into a `bytes24`.
pub struct FunctionPtr {
    /// The contract address that will be used when calling this function
    /// pointer.
    pub address: Address,
    /// The selector of the contract function getting called.
    pub selector: Selector,
}
