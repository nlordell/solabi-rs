//! Solidity ABI type layout.
//!
//! This specifies the different size and "dynamic-ness" of encodable types.

use crate::types::{bytes::Bytes, Primitive};

/// A trait for providing size information for an encodable type.
pub trait Layout {
    /// Returns the size information for the type.
    fn size(&self) -> Size;
}

/// Encoding size.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Size {
    /// Static type size, specifying the number of words required to represent
    /// the type.
    Static(usize),

    /// Dynamic type size, specifying the number of words required to represent
    /// the "head" and the "tail" of the type.
    Dynamic(usize, usize),
}

impl Size {
    /// Combines multiple sizes of fields into the size of their tuple.
    pub fn tuple(fields: impl IntoIterator<Item = Size>) -> Self {
        fields
            .into_iter()
            .fold(Self::Static(0), |acc, size| match (acc, size) {
                (Self::Static(h0), Self::Static(h1)) => Self::Static(h0 + h1),
                (Self::Static(h0), Self::Dynamic(h1, t1)) => Self::Dynamic(h0 + 1, h1 + t1),
                (Self::Dynamic(h0, t0), Self::Static(h1)) => Self::Dynamic(h0 + h1, t0),
                _ => {
                    let (h0, t0) = acc.word_count();
                    let (h1, t1) = size.word_count();
                    Self::Dynamic(h0 + 1, t0 + h1 + t1)
                }
            })
    }

    /// Returns the head and tail word counts required for the spcified size.
    ///
    /// Note that for static types, the tail word count is always 0.
    pub fn word_count(&self) -> (usize, usize) {
        match self {
            Self::Static(head) => (*head, 0),
            Self::Dynamic(head, tail) => (*head, *tail),
        }
    }

    /// Returns the total word count of the head and tail combined.
    pub fn total_word_count(&self) -> usize {
        let (head, tail) = self.word_count();
        head + tail
    }

    /// Returns the byte-length for the specified size.
    pub fn byte_length(&self) -> usize {
        self.total_word_count() * 32
    }

    /// Returns the offset, in bytes, of the tail.
    pub fn tail_byte_offset(&self) -> usize {
        let (head, _) = self.word_count();
        head * 32
    }

    /// Returns `true` if the type is static.
    pub fn is_static(&self) -> bool {
        match self {
            Self::Static(..) => true,
            Self::Dynamic(..) => false,
        }
    }

    /// Returns `true` if the type is dynamic.
    pub fn is_dynamic(&self) -> bool {
        match self {
            Self::Static(..) => false,
            Self::Dynamic(..) => true,
        }
    }
}

impl<T> Layout for T
where
    T: Primitive,
{
    fn size(&self) -> Size {
        Size::Static(1)
    }
}

impl<T, const N: usize> Layout for [T; N]
where
    T: Layout,
{
    fn size(&self) -> Size {
        Size::tuple(self.iter().map(|item| item.size()))
    }
}

impl<T, const N: usize> Layout for &'_ [T; N]
where
    T: Layout,
{
    fn size(&self) -> Size {
        (**self).size()
    }
}

impl<T> Layout for &'_ [T]
where
    T: Layout,
{
    fn size(&self) -> Size {
        let tail = Size::tuple(self.iter().map(|item| item.size())).total_word_count();
        Size::Dynamic(1, tail)
    }
}

impl<T> Layout for Vec<T>
where
    T: Layout,
{
    fn size(&self) -> Size {
        (&**self).size()
    }
}

impl Layout for &'_ str {
    fn size(&self) -> Size {
        Bytes(self.as_bytes()).size()
    }
}

impl Layout for String {
    fn size(&self) -> Size {
        (&**self).size()
    }
}

macro_rules! impl_layout_for_tuple {
    ($($t:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> Layout for ($($t,)*)
        where
            $($t: Layout,)*
        {
            fn size(&self) -> Size {
                let ($($t,)*) = self;
                Size::tuple([
                    $(($t).size(),)*
                ])
            }
        }

        impl<$($t),*> Layout for &'_ ($($t,)*)
        where
            $($t: Layout,)*
        {
            fn size(&self) -> Size {
                (**self).size()
            }
        }
    };
}

impl_layout_for_tuple! {}
impl_layout_for_tuple! { A }
impl_layout_for_tuple! { A, B }
impl_layout_for_tuple! { A, B, C }
impl_layout_for_tuple! { A, B, C, D }
impl_layout_for_tuple! { A, B, C, D, E }
impl_layout_for_tuple! { A, B, C, D, E, F }
impl_layout_for_tuple! { A, B, C, D, E, F, G }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE }
impl_layout_for_tuple! { A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z, AA, AB, AC, AD, AE, AF }
