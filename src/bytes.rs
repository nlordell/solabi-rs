//! Solidity bytes type.

use crate::{
    decode::{Decode, DecodeError, Decoder},
    encode::{Encode, Encoder, Size},
    primitive::{Primitive, Word},
};
use std::ops::{Deref, DerefMut};

/// A wrapper type for bytes.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct Bytes<T>(pub T);

impl<T> AsRef<[u8]> for Bytes<T>
where
    T: AsRef<[u8]>,
{
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl<T> AsMut<[u8]> for Bytes<T>
where
    T: AsMut<[u8]>,
{
    fn as_mut(&mut self) -> &mut [u8] {
        self.0.as_mut()
    }
}

impl<T> Deref for Bytes<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Bytes<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

macro_rules! impl_primitive_for_fixed_bytes {
    ($($n:literal,)*) => {$(
        impl Primitive for Bytes<[u8; $n]> {
            fn to_word(&self) -> Word {
                let mut word = Word::default();
                word[..$n].copy_from_slice(self.as_ref());
                word
            }

            fn cast(word: Word) -> Self {
                let mut bytes = Self::default();
                bytes.copy_from_slice(&word[..$n]);
                bytes
            }
        }
    )*};
}

impl_primitive_for_fixed_bytes! {
     1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16,
    17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
}

impl Encode for Bytes<&'_ [u8]> {
    fn size(&self) -> Size {
        let words = (self.len() + 31) / 32;
        Size::Dynamic(1 + words, 0)
    }

    fn encode(&self, encoder: &mut Encoder) {
        encoder.write(&self.len());
        encoder.write_bytes(self);
    }
}

impl Encode for Bytes<Vec<u8>> {
    fn size(&self) -> Size {
        Bytes(&self[..]).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        Bytes(&self[..]).encode(encoder)
    }
}

impl Decode for Bytes<Vec<u8>> {
    fn is_dynamic() -> bool {
        true
    }

    fn decode(decoder: &mut Decoder) -> Result<Self, DecodeError> {
        let len = decoder.read_size()?;
        Ok(Self(decoder.read_bytes(len)?.to_owned()))
    }
}
