//! Solidity integer types with specific bitwidths.
//!
//! These types are needed because there aren't Rust equivalents for all
//! Solidity integer types (`int96` for example).

use crate::primitive::{Primitive, Word};
use ethprim::{I256, U256};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    mem,
};

/// Implements a Solidity bit-integers.
///
/// This is implemented as a macro for two reasons:
/// - To seal possible Solidity integer types
/// - To allow `const fn` implementations for various functions
macro_rules! impl_bitint {
    ($(impl $t:ident <$s:ident $n:literal> ($i:ident) ;)*) => {$(
        #[doc = concat!(
            "A ", stringify!($n), "-bit Solidity ", stringify!($s), " integer."
        )]
        #[derive(Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd)]
        pub struct $t($i);

        impl $t {
            #[doc = concat!(
                "Creates a new ", stringify!($n), "-bit Solidity integer from ",
                "its underlying value. Returns `None` if the value does ",
                "overflows ", stringify!($n), " bits."
            )]
            pub const fn new(value: $i) -> Option<Self> {
                impl_bitint!(const_new::<$n, $i>(value))
            }

            #[doc = concat!(
                "Creates a new ", stringify!($n), "-bit Solidity integer by ",
                "truncating a value to ", stringify!($n), " bits."
            )]
            pub fn new_truncated(value: $i) -> Self {
                Self(Self::truncate(value))
            }

            #[doc = concat!(
                "Creates a new ", stringify!($n), "-bit Solidity integer from ",
                "its underlying value without checking whether or not it fits ",
                "in ", stringify!($n), " bits."
            )]
            ///
            /// # Safety
            ///
            #[doc = concat!(
                "The caller must ensure that the value fits in ",
                stringify!($n), " bits."
            )]
            pub const unsafe fn new_unchecked(value: $i) -> Self {
                Self(value)
            }

            /// Gets the underlying value.
            pub const fn get(self) -> $i {
                self.0
            }

            /// Truncates a value.
            const fn truncate(value: $i) -> $i {
                impl_bitint!(const_truncate::<$n, $i>(value))
            }
        }

        impl Primitive for $t {
            fn to_word(&self) -> Word {
                self.0.to_word()
            }

            fn from_word(word: Word) -> Self {
                Self::new_truncated(<$i>::from_word(word))
            }
        }

        impl TryFrom<$i> for $t {
            type Error = TryFromIntegerError;

            fn try_from(value: $i) -> Result<Self, Self::Error> {
                Self::new(value).ok_or(TryFromIntegerError)
            }
        }
    )*};

    (const_new::<$n:literal, I256>($v:expr)) => {
        impl_bitint!(const_new256::<$n, I256>($v))
    };
    (const_new::<$n:literal, U256>($v:expr)) => {
        impl_bitint!(const_new256::<$n, U256>($v))
    };
    (const_new256::<$n:literal, $i:ident>($v:expr)) => {{
        let value = $v;
        let (hi, _) = value.into_words();
        let (hi_truncated, _) = Self::truncate(value).into_words();
        if hi == hi_truncated {
            Some(Self(value))
        } else {
            None
        }
    }};
    (const_new::<$n:literal, $i:ident>($v:expr)) => {{
        let value = $v;
        if value == Self::truncate(value) {
            Some(Self(value))
        } else {
            None
        }
    }};

    (const_truncate::<$n:literal, I256>($v:expr)) => {
        impl_bitint!(const_truncate256::<$n, I256>($v))
    };
    (const_truncate::<$n:literal, U256>($v:expr)) => {
        impl_bitint!(const_truncate256::<$n, U256>($v))
    };
    (const_truncate256::<$n:literal, $i:ident>($v:expr)) => {{
        const SHIFT: usize = (mem::size_of::<$i>() * 8) - $n;
        let (hi, lo) = $v.into_words();
        <$i>::from_words((hi << SHIFT) >> SHIFT, lo)
    }};
    (const_truncate::<$n:literal, $i:ident>($v:expr)) => {{
        const SHIFT: usize = (mem::size_of::<$i>() * 8) - $n;
        ($v << SHIFT) >> SHIFT
    }};
}

pub type Int8 = i8;
pub type Int16 = i16;
pub type Int32 = i32;
pub type Int64 = i64;
pub type Int128 = i128;
pub type Int256 = I256;
pub type Uint8 = u8;
pub type Uint16 = u16;
pub type Uint32 = u32;
pub type Uint64 = u64;
pub type Uint128 = u128;
pub type Uint256 = U256;

impl_bitint! {
    impl Int24 <signed 24> (i32);
    impl Int40 <signed 40> (i64);
    impl Int48 <signed 48> (i64);
    impl Int56 <signed 56> (i64);
    impl Int72 <signed 72> (i128);
    impl Int80 <signed 80> (i128);
    impl Int88 <signed 88> (i128);
    impl Int96 <signed 96> (i128);
    impl Int104 <signed 104> (i128);
    impl Int112 <signed 112> (i128);
    impl Int120 <signed 120> (i128);
    impl Int136 <signed 136> (I256);
    impl Int144 <signed 144> (I256);
    impl Int152 <signed 152> (I256);
    impl Int160 <signed 160> (I256);
    impl Int168 <signed 168> (I256);
    impl Int176 <signed 176> (I256);
    impl Int184 <signed 184> (I256);
    impl Int192 <signed 192> (I256);
    impl Int200 <signed 200> (I256);
    impl Int208 <signed 208> (I256);
    impl Int216 <signed 216> (I256);
    impl Int224 <signed 224> (I256);
    impl Int240 <signed 240> (I256);
    impl Int248 <signed 248> (I256);
    impl Uint24 <unsigned 24> (u32);
    impl Uint40 <unsigned 40> (u64);
    impl Uint48 <unsigned 48> (u64);
    impl Uint56 <unsigned 56> (u64);
    impl Uint72 <unsigned 72> (u128);
    impl Uint80 <unsigned 80> (u128);
    impl Uint88 <unsigned 88> (u128);
    impl Uint96 <unsigned 96> (u128);
    impl Uint104 <unsigned 104> (u128);
    impl Uint112 <unsigned 112> (u128);
    impl Uint120 <unsigned 120> (u128);
    impl Uint136 <unsigned 136> (U256);
    impl Uint144 <unsigned 144> (U256);
    impl Uint152 <unsigned 152> (U256);
    impl Uint160 <unsigned 160> (U256);
    impl Uint168 <unsigned 168> (U256);
    impl Uint176 <unsigned 176> (U256);
    impl Uint184 <unsigned 184> (U256);
    impl Uint192 <unsigned 192> (U256);
    impl Uint200 <unsigned 200> (U256);
    impl Uint208 <unsigned 208> (U256);
    impl Uint216 <unsigned 216> (U256);
    impl Uint224 <unsigned 224> (U256);
    impl Uint240 <unsigned 240> (U256);
    impl Uint248 <unsigned 248> (U256);
}

/// An error converting to Solidity integer type from its underlying
/// representation.
#[derive(Debug)]
pub struct TryFromIntegerError;

impl Display for TryFromIntegerError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("value overflows integer")
    }
}

impl Error for TryFromIntegerError {}
