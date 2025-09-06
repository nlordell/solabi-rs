//! Module implementing an EVM log datatype.

use crate::{
    bytes::Bytes,
    primitive::{Primitive, Word},
};
use ethprim::Hasher;
use std::{
    array::TryFromSliceError,
    borrow::{Borrow, BorrowMut, Cow},
    fmt::{self, Debug, Formatter},
    hash::{self, Hash},
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

    /// Tries to append a topic as a [`Word`].
    ///
    /// Returns `Err` if the topics are full.
    pub fn try_push_word(&mut self, topic: Word) -> Result<(), Word> {
        if self.len >= Topics::MAX_LEN {
            return Err(topic);
        }
        self.values[self.len].write(topic);
        self.len += 1;
        Ok(())
    }

    /// Appends a topic as a [`Word`].
    ///
    /// # Panics
    ///
    /// This method will panic if the topics are full.
    pub fn push_word(&mut self, topic: Word) {
        self.try_push_word(topic)
            .unwrap_or_else(|_| panic!("topics are full"));
    }

    /// Tries to append a topic.
    ///
    /// Returns `Err` if the topics are full.
    pub fn try_push(&mut self, topic: &impl ToTopic) -> Result<(), Word> {
        self.try_push_word(topic.to_topic())
    }

    /// Appends a topic.
    ///
    /// # Panics
    ///
    /// This method will panic if the topics are full.
    pub fn push(&mut self, topic: &impl ToTopic) {
        self.push_word(topic.to_topic())
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
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
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

/// A trait for converting values to topics.
pub trait ToTopic {
    /// Returns the value as a EVM log topic.
    fn to_topic(&self) -> Word;
}

/// A trait for converting values from topics.
pub trait FromTopic: Sized {
    /// Returns the value from a EVM log topic. Returns `None` if the topic
    /// does not encode a valid value.
    fn from_topic(topic: Word) -> Option<Self>;
}

/// A trait for hashing fields for topic computation in dynamic types.
pub trait TopicHash {
    /// Update the hasher with topic values.
    fn update_hash(&self, hasher: &mut Hasher);
}

impl<T> ToTopic for T
where
    T: Primitive,
{
    fn to_topic(&self) -> Word {
        self.to_word()
    }
}

impl<T> FromTopic for T
where
    T: Primitive,
{
    fn from_topic(topic: Word) -> Option<Self> {
        Self::from_word(topic)
    }
}

impl<T> TopicHash for T
where
    T: Primitive,
{
    fn update_hash(&self, hasher: &mut Hasher) {
        hasher.update(self.to_word());
    }
}

impl<T> ToTopic for Cow<'_, T>
where
    T: ToTopic + ToOwned + ?Sized,
{
    fn to_topic(&self) -> Word {
        self.as_ref().to_topic()
    }
}

impl<T> FromTopic for Cow<'_, T>
where
    T: ToOwned + ?Sized,
    T::Owned: FromTopic,
{
    fn from_topic(topic: Word) -> Option<Self> {
        T::Owned::from_topic(topic).map(Cow::Owned)
    }
}

impl<T> TopicHash for Cow<'_, T>
where
    T: TopicHash + ToOwned + ?Sized,
{
    fn update_hash(&self, hasher: &mut Hasher) {
        self.as_ref().update_hash(hasher);
    }
}

macro_rules! impl_topic_for_ref {
    (to_topic) => {
        fn to_topic(&self) -> Word {
            (**self).to_topic()
        }
    };
    (update_hash) => {
        fn update_hash(&self, hasher: &mut Hasher) {
            (**self).update_hash(hasher)
        }
    };
}

impl<T, const N: usize> ToTopic for [T; N]
where
    T: TopicHash,
{
    fn to_topic(&self) -> Word {
        self.as_slice().to_topic()
    }
}

impl<T, const N: usize> TopicHash for [T; N]
where
    T: TopicHash,
{
    fn update_hash(&self, hasher: &mut Hasher) {
        self.as_slice().update_hash(hasher)
    }
}

impl<T, const N: usize> ToTopic for &'_ [T; N]
where
    T: TopicHash,
{
    impl_topic_for_ref!(to_topic);
}

impl<T, const N: usize> TopicHash for &'_ [T; N]
where
    T: TopicHash,
{
    impl_topic_for_ref!(update_hash);
}

impl<T> ToTopic for [T]
where
    T: TopicHash,
{
    fn to_topic(&self) -> Word {
        let mut hasher = Hasher::new();
        self.update_hash(&mut hasher);
        *hasher.finalize()
    }
}

impl<T> TopicHash for [T]
where
    T: TopicHash,
{
    fn update_hash(&self, hasher: &mut Hasher) {
        for item in self {
            item.update_hash(hasher);
        }
    }
}

impl<T> ToTopic for &'_ [T]
where
    T: TopicHash,
{
    impl_topic_for_ref!(to_topic);
}

impl<T> TopicHash for &'_ [T]
where
    T: TopicHash,
{
    impl_topic_for_ref!(update_hash);
}

impl<T> ToTopic for Vec<T>
where
    T: TopicHash,
{
    fn to_topic(&self) -> Word {
        self.as_slice().to_topic()
    }
}

impl<T> FromTopic for Vec<T> {
    fn from_topic(_: Word) -> Option<Self> {
        Some(Self::default())
    }
}

impl<T> TopicHash for Vec<T>
where
    T: TopicHash,
{
    fn update_hash(&self, hasher: &mut Hasher) {
        self.as_slice().update_hash(hasher)
    }
}

impl ToTopic for str {
    fn to_topic(&self) -> Word {
        Bytes(self.as_bytes()).to_topic()
    }
}

impl TopicHash for str {
    fn update_hash(&self, hasher: &mut Hasher) {
        Bytes(self.as_bytes()).update_hash(hasher)
    }
}

impl ToTopic for &'_ str {
    impl_topic_for_ref!(to_topic);
}

impl TopicHash for &'_ str {
    impl_topic_for_ref!(update_hash);
}

impl ToTopic for String {
    fn to_topic(&self) -> Word {
        self.as_str().to_topic()
    }
}

impl FromTopic for String {
    fn from_topic(_: Word) -> Option<Self> {
        Some(Self::default())
    }
}

impl TopicHash for String {
    fn update_hash(&self, hasher: &mut Hasher) {
        self.as_str().update_hash(hasher);
    }
}

macro_rules! impl_to_from_topic_for_tuple {
    ($($t:ident),*) => {
        impl<$($t),*> ToTopic for ($($t,)*)
        where
            $($t: TopicHash,)*
        {
            fn to_topic(&self) -> Word {
                let mut hasher = Hasher::new();
                self.update_hash(&mut hasher);
                *hasher.finalize()
            }
        }

        #[allow(clippy::unused_unit)]
        impl<$($t),*> FromTopic for ($($t,)*)
        where
            $($t: FromTopic,)*
        {
            fn from_topic(_: Word) -> Option<Self> {
                Some(($($t::from_topic([0; 32])?,)*))
            }
        }

        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> TopicHash for ($($t,)*)
        where
            $($t: TopicHash,)*
        {
            fn update_hash(&self, hasher: &mut Hasher) {
                let ($($t,)*) = self;
                $($t.update_hash(hasher);)*
            }
        }

        impl<$($t),*> ToTopic for &'_ ($($t,)*)
        where
            $($t: TopicHash,)*
        {
            impl_topic_for_ref!(to_topic);
        }

        impl<$($t),*> TopicHash for &'_ ($($t,)*)
        where
            $($t: TopicHash,)*
        {
            impl_topic_for_ref!(update_hash);
        }
    };
}

impl_to_from_topic_for_tuple! {}
impl_to_from_topic_for_tuple! { A }
impl_to_from_topic_for_tuple! { A, B }
impl_to_from_topic_for_tuple! { A, B, C }
impl_to_from_topic_for_tuple! { A, B, C, D }
impl_to_from_topic_for_tuple! { A, B, C, D, E }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE }
impl_to_from_topic_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF }
