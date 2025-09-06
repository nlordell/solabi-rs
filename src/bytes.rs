//! Solidity bytes type.

use crate::{
    decode::{Decode, DecodeError, Decoder},
    encode::{Encode, Encoder, Size},
    encode_packed::EncodePacked,
    fmt::Hex,
    log::{FromTopic, ToTopic, TopicHash},
    primitive::{Primitive, Word},
};
use ethprim::Hasher;
use std::{
    borrow::{Borrow, Cow},
    fmt::{self, Debug, Formatter},
    mem,
    ops::{Deref, DerefMut},
};

/// A wrapper type for bytes.
#[derive(Clone, Copy, Default, Eq, PartialEq)]
#[repr(transparent)]
pub struct Bytes<T>(pub T)
where
    T: ?Sized;

impl Bytes<[u8]> {
    /// Returns a borrowed `Bytes` from a slice of bytes.
    #[allow(clippy::needless_lifetimes)]
    pub const fn new<'a>(bytes: &'a [u8]) -> &'a Bytes<[u8]> {
        // SAFETY: DSTs are a bit of a mystery to me... To my understanding
        // this should be safe because `Bytes` has a transparent layout, so
        // `&[u8]` and `&Bytes<[u8]>`. Either way, we should get a fat pointer
        // with the correct length and pointing to the start of the slice and
        // transmuting between them should be safe.
        unsafe { mem::transmute(bytes) }
    }

    /// Returns a new `Cow::Borrowed` slice of bytes.
    #[allow(clippy::needless_lifetimes)]
    pub const fn borrowed<'a>(bytes: &'a [u8]) -> Cow<'a, Bytes<[u8]>> {
        Cow::Borrowed(Self::new(bytes))
    }
}

impl<T> Bytes<T>
where
    T: AsRef<[u8]> + ?Sized,
{
    /// Returns the underlying slice of bytes.
    pub fn as_bytes(&self) -> &[u8] {
        self.as_ref()
    }
}

impl<T> Bytes<T>
where
    T: AsMut<[u8]> + ?Sized,
{
    /// Returns a mutable reference to the underlying slice of bytes.
    pub fn as_bytes_mut(&mut self) -> &[u8] {
        self.as_mut()
    }
}

impl Bytes<Vec<u8>> {
    /// Returns a borrowed `Bytes`.
    pub fn as_borrowed(&self) -> &Bytes<[u8]> {
        Bytes::new(&self[..])
    }
}

impl<T> AsRef<[u8]> for Bytes<T>
where
    T: AsRef<[u8]> + ?Sized,
{
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl<T> AsMut<[u8]> for Bytes<T>
where
    T: AsMut<[u8]> + ?Sized,
{
    fn as_mut(&mut self) -> &mut [u8] {
        self.0.as_mut()
    }
}

impl<T> Debug for Bytes<T>
where
    T: AsRef<[u8]> + ?Sized,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Bytes").field(&Hex(self.0.as_ref())).finish()
    }
}

impl<T> Deref for Bytes<T>
where
    T: ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Bytes<T>
where
    T: ?Sized,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Borrow<Bytes<[u8]>> for Bytes<Vec<u8>> {
    fn borrow(&self) -> &Bytes<[u8]> {
        self.as_borrowed()
    }
}

impl ToOwned for Bytes<[u8]> {
    type Owned = Bytes<Vec<u8>>;

    fn to_owned(&self) -> Self::Owned {
        Bytes(self.0.to_owned())
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

            fn from_word(word: Word) -> Self {
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

impl Encode for Bytes<[u8]> {
    fn size(&self) -> Size {
        Bytes(&self.0).size()
    }

    fn encode(&self, encoder: &mut Encoder) {
        Bytes(&self.0).encode(encoder)
    }
}

impl ToTopic for Bytes<[u8]> {
    fn to_topic(&self) -> Word {
        Bytes(&self.0).to_topic()
    }
}

impl TopicHash for Bytes<[u8]> {
    fn update_hash(&self, hasher: &mut Hasher) {
        Bytes(&self.0).update_hash(hasher);
    }
}

impl Encode for Bytes<&'_ [u8]> {
    fn size(&self) -> Size {
        let words = self.len().div_ceil(32);
        Size::Dynamic(1 + words, 0)
    }

    fn encode(&self, encoder: &mut Encoder) {
        encoder.write(&self.len());
        encoder.write_bytes(self);
    }
}

impl<T> EncodePacked for Bytes<T>
where
    T: AsRef<[u8]> + ?Sized,
{
    fn packed_size(&self) -> usize {
        self.as_bytes().len()
    }
    fn encode_packed(&self, out: &mut [u8]) {
        out[..self.as_bytes().len()].copy_from_slice(self.as_bytes());
    }
}

impl ToTopic for Bytes<&'_ [u8]> {
    fn to_topic(&self) -> Word {
        let mut hasher = Hasher::new();
        hasher.update(self);
        *hasher.finalize()
    }
}

impl TopicHash for Bytes<&'_ [u8]> {
    fn update_hash(&self, hasher: &mut Hasher) {
        hasher.update(self);

        static ZEROS: Word = [0; 32];
        let padding = (32 - (self.len() % 32)) % 32;
        hasher.update(&ZEROS[..padding]);
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

impl ToTopic for Bytes<Vec<u8>> {
    fn to_topic(&self) -> Word {
        Bytes(&self[..]).to_topic()
    }
}

impl FromTopic for Bytes<Vec<u8>> {
    fn from_topic(_: Word) -> Self {
        Self::default()
    }
}

impl TopicHash for Bytes<Vec<u8>> {
    fn update_hash(&self, hasher: &mut Hasher) {
        Bytes(&self[..]).update_hash(hasher);
    }
}
