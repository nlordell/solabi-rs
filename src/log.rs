//! Module implementing an EVM log datatype.

use crate::primitive::Word;
use std::{
    array::TryFromSliceError,
    borrow::{Borrow, BorrowMut, Cow},
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    mem::MaybeUninit,
    ops::{Deref, DerefMut, Index, IndexMut},
    slice::SliceIndex,
};

/// An EVM log.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Log<'a> {
    /// The log topics.
    pub topics: Topics,

    /// The log data.
    pub data: Cow<'a, [u8]>,
}

impl Log<'_> {
    /// Creates an owned log from a reference.
    pub fn to_owned(&self) -> Log<'static> {
        Log {
            topics: self.topics,
            data: self.data.clone().into_owned().into(),
        }
    }
}

/// An array of log topics.
///
/// This specialized `Topics` type exists in order to store the topics in a
/// fixed-size array, since there are at most 4 topics per log. This allows the
/// topics to be stored without a heap allocation, and guarantees that it is
/// the correct length.
#[derive(Clone, Copy)]
pub struct Topics {
    len: usize,
    values: [MaybeUninit<Word>; Topics::MAX_LEN],
}

impl Topics {
    /// Returns the maximum length of topics.
    pub const MAX_LEN: usize = 4;

    /// Returns the topics as a slice of words.
    pub fn as_slice(&self) -> &[Word] {
        // SAFETY: We garantee that the first `self.len` words are initialized.
        // The pointer casting is safe as per the unstable implementation of
        // `MaybeUninit<T>::slice_assume_init_ref`.
        unsafe { &*(&self.values[..self.len] as *const [_] as *const [Word]) }
    }

    /// Returns a mutable slice of topics.
    pub fn as_slice_mut(&mut self) -> &mut [Word] {
        unsafe { &mut *(&mut self.values[..self.len] as *mut [_] as *mut [Word]) }
    }

    /// Tries to append a topic.
    ///
    /// Returns `Err` if the topics are full.
    pub fn try_push(&mut self, topic: Word) -> Result<(), Word> {
        if self.len >= Topics::MAX_LEN {
            return Err(topic);
        }
        self.values[self.len].write(topic);
        self.len += 1;
        Ok(())
    }

    /// Appends a topic.
    ///
    /// # Panics
    ///
    /// This method will panic if the topics are full.
    pub fn push(&mut self, topic: Word) {
        self.try_push(topic).expect("topics are full");
    }
}

impl AsRef<[Word]> for Topics {
    fn as_ref(&self) -> &[Word] {
        self.as_slice()
    }
}

impl AsMut<[Word]> for Topics {
    fn as_mut(&mut self) -> &mut [Word] {
        self.as_slice_mut()
    }
}

impl Borrow<[Word]> for Topics {
    fn borrow(&self) -> &[Word] {
        self.as_slice()
    }
}

impl BorrowMut<[Word]> for Topics {
    fn borrow_mut(&mut self) -> &mut [Word] {
        self.as_slice_mut()
    }
}

impl Debug for Topics {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("Topics").field(&self.as_slice()).finish()
    }
}

impl Default for Topics {
    fn default() -> Self {
        Self {
            len: 0,
            values: [MaybeUninit::uninit(); 4],
        }
    }
}

impl Deref for Topics {
    type Target = [Word];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl DerefMut for Topics {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_slice_mut()
    }
}

impl Eq for Topics {}

impl Hash for Topics {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state)
    }
}

impl<I: SliceIndex<[Word]>> Index<I> for Topics {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        self.as_slice().index(index)
    }
}

impl<I: SliceIndex<[Word]>> IndexMut<I> for Topics {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.as_slice_mut().index_mut(index)
    }
}

impl IntoIterator for Topics {
    type Item = Word;
    type IntoIter = TopicsIter;

    fn into_iter(self) -> Self::IntoIter {
        TopicsIter {
            index: 0,
            topics: self,
        }
    }
}

/// An iterator over topics.
pub struct TopicsIter {
    index: usize,
    topics: Topics,
}

impl Iterator for TopicsIter {
    type Item = Word;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.topics.get(self.index)?;
        self.index += 1;
        Some(*next)
    }
}

impl PartialEq for Topics {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialEq<[Word]> for Topics {
    fn eq(&self, other: &[Word]) -> bool {
        self.as_slice() == other
    }
}

impl PartialEq<&[Word]> for Topics {
    fn eq(&self, other: &&[Word]) -> bool {
        self.as_slice() == *other
    }
}

impl PartialEq<&mut [Word]> for Topics {
    fn eq(&self, other: &&mut [Word]) -> bool {
        self.as_slice() == *other
    }
}

impl TryFrom<&[Word]> for Topics {
    type Error = TryFromSliceError;

    fn try_from(value: &[Word]) -> Result<Self, Self::Error> {
        if value.len() > 4 {
            // Work around not being able to construct a `TryFromSliceError`.
            return <[Word; 0]>::try_from(value).map(Self::from);
        }

        let mut values = [MaybeUninit::uninit(); 4];
        for (i, topic) in value.iter().copied().enumerate() {
            values[i].write(topic);
        }
        Ok(Self {
            len: value.len(),
            values,
        })
    }
}

impl TryFrom<&mut [Word]> for Topics {
    type Error = TryFromSliceError;

    fn try_from(value: &mut [Word]) -> Result<Self, Self::Error> {
        (&*value).try_into()
    }
}

impl TryFrom<Vec<Word>> for Topics {
    type Error = TryFromSliceError;

    fn try_from(value: Vec<Word>) -> Result<Self, Self::Error> {
        value[..].try_into()
    }
}

impl TryFrom<Box<[Word]>> for Topics {
    type Error = TryFromSliceError;

    fn try_from(value: Box<[Word]>) -> Result<Self, Self::Error> {
        value[..].try_into()
    }
}

macro_rules! impl_array {
    ($($n:tt),*) => {$(
        impl From<[Word; $n]> for Topics {
            fn from(topics: [Word; $n]) -> Self {
                let mut values = [MaybeUninit::uninit(); 4];
                for (i, topic) in topics.into_iter().enumerate() {
                    values[i].write(topic);
                }
                Self { len: $n, values }
            }
        }

        impl PartialEq<[Word; $n]> for Topics {
            fn eq(&self, other: &[Word; $n]) -> bool {
                self.as_slice() == other
            }
        }

        impl PartialEq<&[Word; $n]> for Topics {
            fn eq(&self, other: &&[Word; $n]) -> bool {
                self.as_slice() == *other
            }
        }

        impl TryFrom<Topics> for [Word; $n] {
            type Error = TryFromSliceError;

            fn try_from(topics: Topics) -> Result<Self, Self::Error> {
                topics.as_slice().try_into()
            }
        }

        impl TryFrom<&Topics> for [Word; $n] {
            type Error = TryFromSliceError;

            fn try_from(topics: &Topics) -> Result<Self, Self::Error> {
                topics.as_slice().try_into()
            }
        }

        impl<'a> TryFrom<&'a Topics> for &'a [Word; $n] {
            type Error = TryFromSliceError;

            fn try_from(topics: &'a Topics) -> Result<Self, Self::Error> {
                topics.as_slice().try_into()
            }
        }
    )*};
}

impl_array! { 0, 1, 2, 3, 4 }
