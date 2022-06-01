//! Solidity ABI encoding and decoding implementation.

pub mod bytes;
pub mod decode;
pub mod encode;
pub mod function;
pub mod primitive;
pub mod value;

pub use self::{decode::decode, encode::encode};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
