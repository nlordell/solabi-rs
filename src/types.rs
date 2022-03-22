//! Solidity ABI specific types.

pub mod address;
pub mod bytes;
pub mod function;
pub mod integer;
pub mod value;

/// An Ethereum 32-byte word.
pub type Word = [u8; 32];

/// Trait reprenting any type that can be converted to and from a single
/// Ethereum 32-byte word.
pub trait Primitive {
    /// Converts a primitive type to an Ethereum 32-byte word.
    fn to_word(&self) -> Word;
}
