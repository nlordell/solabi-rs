//! Module containing dynamic Solidity value.

use crate::{
    encode::{Encode, Encoder, Size},
    function::FunctionPtr,
    primitive::Word,
};
use ethaddr::Address;
use ethnum::{I256, U256};
use std::ops::{Deref, Shl, Shr};

/// A Solidity value.
///
/// This type is capabable of represenging all Solidity values dynamically,
/// allowing for "dynamic" encoding and decoding. This is analogous to
/// `serde_json::Value` for Solidity values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Int(Int),
    Uint(Uint),
    Address(Address),
    Bool(bool),
    FixedBytes(FixedBytes),
    Function(FunctionPtr),
    FixedArray(Array),
    Bytes(Vec<u8>),
    String(String),
    Array(Array),
    Tuple(Vec<Value>),
}

impl Value {
    /// Returns the kind for the value.
    pub fn kind(&self) -> ValueKind {
        match self {
            Self::Int(value) => ValueKind::Int(value.bit_width()),
            Self::Uint(value) => ValueKind::Uint(value.bit_width()),
            Self::Address(_) => ValueKind::Address,
            Self::Bool(_) => ValueKind::Bool,
            Self::FixedBytes(value) => ValueKind::FixedBytes(value.byte_length()),
            Self::Function(_) => ValueKind::Function,
            Self::FixedArray(value) => {
                ValueKind::FixedArray(value.len(), Box::new(value.element_kind().clone()))
            }
            Self::Bytes(_) => ValueKind::Bytes,
            Self::String(_) => ValueKind::String,
            Self::Array(value) => ValueKind::Array(Box::new(value.element_kind().clone())),
            Self::Tuple(value) => ValueKind::Tuple(value.iter().map(|v| v.kind()).collect()),
        }
    }

    /// Encodes a `Value`.
    ///
    /// Note that `Value`s can't use the [`crate::encode`] method directly as
    /// it would cause inconsistent behaviour. For example, `Vec<Value>` is not
    /// always a valid Solidity type (which is why the [`Array`] abstraction
    /// exists).
    pub fn encode(&self) -> Vec<u8> {
        crate::encode(Encodable(self))
    }
}

/// Internal type for implementing encoding on [`Value`]s.
struct Encodable<'a, T>(&'a T);

impl Encode for Encodable<'_, Value> {
    fn size(&self) -> Size {
        match self.0 {
            Value::Int(value) => value.size(),
            Value::Uint(value) => value.size(),
            Value::Address(value) => value.size(),
            Value::Bool(value) => value.size(),
            Value::FixedBytes(value) => value.1.size(),
            Value::Function(value) => value.size(),
            Value::Bytes(value) => value.size(),
            Value::String(value) => value.size(),
            Value::Array(Array(_, values)) => {
                Size::dynamic_array(values.iter().map(|item| Encodable(item).size()))
            }
            Value::FixedArray(Array(_, values)) | Value::Tuple(values) => {
                Size::tuple(values.iter().map(|item| Encodable(item).size()))
            }
        }
    }

    fn encode(&self, encoder: &mut Encoder) {
        match self.0 {
            Value::Int(value) => value.encode(encoder),
            Value::Uint(value) => value.encode(encoder),
            Value::Address(value) => value.encode(encoder),
            Value::Bool(value) => value.encode(encoder),
            Value::FixedBytes(value) => value.1.encode(encoder),
            Value::Function(value) => value.encode(encoder),
            Value::Bytes(value) => value.encode(encoder),
            Value::String(value) => value.encode(encoder),
            Value::Array(Array(_, values)) => {
                // TODO(nlordell): Slight code-smell as we have duplicated this
                // logic from `<&[T] as Encode>::encode`.
                encoder.write(&values.len());
                let inner_size = Size::tuple(values.iter().map(|item| Encodable(item).size()));
                let mut inner = encoder.untail(inner_size);
                for item in values {
                    inner.write(&Encodable(item))
                }
            }
            Value::FixedArray(Array(_, values)) | Value::Tuple(values) => {
                for value in values {
                    encoder.write(&Encodable(value))
                }
            }
        }
    }
}

/// The kind of the Solidity value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueKind {
    Int(BitWidth),
    Uint(BitWidth),
    Address,
    Bool,
    FixedBytes(ByteLength),
    Function,
    FixedArray(usize, Box<ValueKind>),
    Bytes,
    String,
    Array(Box<ValueKind>),
    Tuple(Vec<ValueKind>),
}

/// An intenger bit width.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BitWidth(u32);

impl BitWidth {
    /// Checked bit-width creation.
    ///
    /// Bitwidths must be multiple of 8s between 8 and 256.
    pub fn new(bit_width: u32) -> Option<Self> {
        match bit_width {
            n @ 8..=256 if n % 8 == 0 => Some(Self(bit_width)),
            _ => None,
        }
    }

    /// Resizes an integer to fit in `N` bits.
    fn resize<T>(self, value: T) -> Option<T>
    where
        T: Copy + Eq + Shl<u32, Output = T> + Shr<u32, Output = T>,
    {
        let shift = 256 - self.0;
        let masked = (value << shift) >> shift;
        if masked == value {
            Some(value)
        } else {
            None
        }
    }
}

impl Default for BitWidth {
    fn default() -> Self {
        Self(256)
    }
}

impl Deref for BitWidth {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// An signed integer with the specified width.
///
/// This type is needed because there aren't Rust equivalents for all Solidity
/// integer types (`int96` for example).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Int(BitWidth, I256);

impl Int {
    /// Creates a new signed integer.
    ///
    /// # Panics
    ///
    /// Panics on invalid bit-width.
    pub fn new(bit_width: u32, value: I256) -> Option<Self> {
        let bit_width = BitWidth::new(bit_width)?;
        let value = bit_width.resize(value)?;
        Some(Self(bit_width, value))
    }

    /// Returns the integer's bit width.
    pub fn bit_width(&self) -> BitWidth {
        self.0
    }
}

impl Deref for Int {
    type Target = I256;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

/// An unsigned integer with the specified width.
///
/// This type is needed because there aren't Rust equivalents for all Solidity
/// integer types (`uint96` for example).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Uint(BitWidth, U256);

impl Uint {
    /// Creates a new unsigned integer.
    ///
    /// # Panics
    ///
    /// Panics on invalid bit-width.
    pub fn new(bit_width: u32, value: U256) -> Option<Self> {
        let bit_width = BitWidth::new(bit_width)?;
        let value = bit_width.resize(value)?;
        Some(Self(bit_width, value))
    }

    /// Returns the integer's bit width.
    pub fn bit_width(&self) -> BitWidth {
        self.0
    }
}

impl Deref for Uint {
    type Target = U256;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

/// A fixes byte array length.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ByteLength(usize);

impl ByteLength {
    /// Checked bit-width creation.
    ///
    /// Bitwidths must be multiple of 8s between 8 and 256.
    pub fn new(len: usize) -> Option<Self> {
        match len {
            1..=32 => Some(Self(len)),
            _ => None,
        }
    }
}

impl Deref for ByteLength {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A dynamic value for reprensenting Solidity fixed bytes type.
///
/// This is different than using [`crate::types::bytes::Bytes`] in that the size
/// is not known at compile time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FixedBytes(ByteLength, [u8; 32]);

impl FixedBytes {
    /// Creates a new fixed byte value.
    pub fn new(bytes: &[u8]) -> Option<Self> {
        let len = ByteLength::new(bytes.len())?;
        let mut word = Word::default();
        word[..*len].copy_from_slice(bytes);
        Some(Self(len, word))
    }

    /// Returns the byte length of the fixed bytes.
    pub fn byte_length(&self) -> ByteLength {
        // Already verified by the constructor.
        ByteLength(self.len())
    }
}

impl Deref for FixedBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

/// An array of values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Array(ValueKind, Vec<Value>);

impl Array {
    /// Creates a new fixed byte value.
    pub fn new(values: Vec<Value>) -> Option<Self> {
        let mut iter = values.iter();
        let element_kind = iter.next()?.kind();
        if iter.any(|value| value.kind() != element_kind) {
            return None;
        }
        Some(Self(element_kind, values))
    }

    /// Creates an empty array.
    pub fn empty(element_kind: ValueKind) -> Self {
        Self(element_kind, Vec::new())
    }

    /// Returns the value kind of the array element.
    pub fn element_kind(&self) -> &ValueKind {
        &self.0
    }
}

impl Deref for Array {
    type Target = [Value];

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

// TODO(nlordell): Implement these traits.
//pub trait ToValue {
//    fn to_value(&self) -> Value;
//}
//pub trait FromValue {
//    fn from_value(&value: Value) -> Self;
//}

// TODO(nlordell): Test this madness.
