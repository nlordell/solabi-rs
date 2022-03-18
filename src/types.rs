//! Solidity ABI specific types.

pub mod address;
pub mod bytes;
pub mod function;
pub mod integer;

/// An Ethereum 32-byte word.
pub type Word = [u8; 32];

/// Trait reprenting any type that can be converted to and from a single
/// Ethereum 32-byte word.
trait Primitive {
    fn to_word(&self) -> Word;
}
