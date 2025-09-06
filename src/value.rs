//! Module containing dynamic Solidity value.

mod encoders;
mod tuple;

pub use self::encoders::*;
use crate::{
    bytes::Bytes,
    decode::{
        context::{self, DecodeContext},
        Decode as _, DecodeError, Decoder,
    },
    encode::{BufferSizeError, Encode, Encoder, Size},
    function::{ExternalFunction, Selector},
    log::TopicHash as _,
    primitive::{Primitive, Word},
};
use ethprim::{Address, Hasher, I256, U256};
use std::{
    fmt::{self, Display, Formatter},
    ops::{Shl, Shr},
    str::FromStr,
};

/// A Solidity value.
///
/// This type is capabable of represenging all Solidity values dynamically,
/// allowing for "dynamic" encoding and decoding. This is analogous to
/// `serde_json::Value` for Solidity values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    /// A signed integer value with a fixed bit-width.
    Int(Int),
    /// An unsigned integer value with a fixed bit-width.
    Uint(Uint),
    /// An Ethereum public address.
    Address(Address),
    /// A boolean.
    Bool(bool),
    /// A fixed-length byte array.
    FixedBytes(FixedBytes),
    /// A Solidity external function pointer.
    Function(ExternalFunction),
    /// A fixed-length value array.
    FixedArray(Array),
    /// A dynamic-length byte array.
    Bytes(Vec<u8>),
    /// A UTF-8 string.
    String(String),
    /// A dynamic-length value array.
    Array(Array),
    /// A tuple of values.
    Tuple(Vec<Value>),
}

impl Value {
    /// Returns the default value for the specified kind.
    pub fn default(kind: &ValueKind) -> Self {
        match kind {
            ValueKind::Int(bit_width) => Self::Int(Int(*bit_width, Default::default())),
            ValueKind::Uint(bit_width) => Self::Uint(Uint(*bit_width, Default::default())),
            ValueKind::Address => Self::Address(Default::default()),
            ValueKind::Bool => Self::Bool(Default::default()),
            ValueKind::FixedBytes(byte_length) => {
                Self::FixedBytes(FixedBytes(*byte_length, Default::default()))
            }
            ValueKind::Function => Self::Function(Default::default()),
            ValueKind::FixedArray(n, kind) => Self::FixedArray(Array(
                *kind.clone(),
                (0..*n).map(|_| Value::default(kind)).collect(),
            )),
            ValueKind::Bytes => Self::Bytes(Default::default()),
            ValueKind::String => Self::String(Default::default()),
            ValueKind::Array(kind) => Self::Array(Array(*kind.clone(), Default::default())),
            ValueKind::Tuple(fields) => Self::Tuple(fields.iter().map(Value::default).collect()),
        }
    }

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

    /// Returns true if the value if of the specified `kind`.
    ///
    /// This is an optimized version of `self.kind() == kind` that does not
    /// require additional allocations.
    pub fn is_kind(&self, kind: &ValueKind) -> bool {
        match (self, kind) {
            (Value::Int(Int(a, _)), ValueKind::Int(b)) => a == b,
            (Value::Uint(Uint(a, _)), ValueKind::Uint(b)) => a == b,
            (Value::Address(_), ValueKind::Address) => true,
            (Value::Bool(_), ValueKind::Bool) => true,
            (Value::FixedBytes(FixedBytes(a, _)), ValueKind::FixedBytes(b)) => a == b,
            (Value::Function(_), ValueKind::Function) => true,
            (Value::FixedArray(a), ValueKind::FixedArray(b, t)) => {
                a.len() == *b && a.element_kind() == &**t
            }
            (Value::Bytes(_), ValueKind::Bytes) => true,
            (Value::String(_), ValueKind::String) => true,
            (Value::Array(a), ValueKind::Array(t)) => a.element_kind() == &**t,
            (Value::Tuple(a), ValueKind::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(v, k)| v.is_kind(k))
            }
            _ => false,
        }
    }

    /// Casts the value to a word if it is a primitive type.
    pub fn to_word(&self) -> Option<Word> {
        match self {
            Value::Int(a) => Some(a.1.to_word()),
            Value::Uint(a) => Some(a.1.to_word()),
            Value::Address(a) => Some(a.to_word()),
            Value::Bool(a) => Some(a.to_word()),
            Value::FixedBytes(a) => Some(a.into_word()),
            Value::Function(a) => Some(a.to_word()),
            _ => None,
        }
    }

    /// Casts a word into a value by kind.
    pub fn from_word(kind: &ValueKind, word: Word) -> Option<Self> {
        match kind {
            ValueKind::Int(bit_width) => {
                Some(Self::Int(Int(*bit_width, Primitive::from_word(word)?)))
            }
            ValueKind::Uint(bit_width) => {
                Some(Self::Uint(Uint(*bit_width, Primitive::from_word(word)?)))
            }
            ValueKind::Address => Some(Self::Address(Primitive::from_word(word)?)),
            ValueKind::Bool => Some(Self::Bool(Primitive::from_word(word)?)),
            ValueKind::FixedBytes(byte_length) => {
                Some(Self::FixedBytes(FixedBytes(*byte_length, word)))
            }
            ValueKind::Function => Some(Self::Function(Primitive::from_word(word)?)),
            _ => None,
        }
    }

    /// Returns the topic value for the specified value.
    pub fn to_topic(&self) -> Word {
        if let Some(word) = self.to_word() {
            return word;
        }

        let mut hasher = Hasher::new();

        // For some odd reason, indexed bytes and strings behave differently
        // when they are alone than when they are elements in an array or fields
        // in a tuple...
        match self {
            Value::Bytes(a) => hasher.update(a),
            Value::String(a) => hasher.update(a),
            _ => self.topic_update_hash(&mut hasher),
        }

        *hasher.finalize()
    }

    /// Writes packed value representation to the specified hasher.
    fn topic_update_hash(&self, hasher: &mut Hasher) {
        let hash_array = |a: &[Self], hasher: &mut Hasher| {
            for i in a {
                i.topic_update_hash(hasher)
            }
        };

        match self {
            Value::Int(a) => hasher.update(a.1.to_word()),
            Value::Uint(a) => hasher.update(a.1.to_word()),
            Value::Address(a) => hasher.update(a.to_word()),
            Value::Bool(a) => hasher.update(a.to_word()),
            Value::FixedBytes(a) => hasher.update(a.into_word()),
            Value::Function(a) => hasher.update(a.to_word()),
            Value::FixedArray(a) | Value::Array(a) => hash_array(a.as_slice(), hasher),
            Value::Bytes(a) => Bytes(&a[..]).update_hash(hasher),
            Value::String(a) => a.update_hash(hasher),
            Value::Tuple(a) => hash_array(a, hasher),
        }
    }

    /// Returns the encoded size of the value.
    pub fn size(&self) -> Size {
        Encodable(self).size()
    }

    /// Encodes a `Value`.
    ///
    /// Note that `Value`s can't use the [`crate::encode()`] method directly as
    /// it would cause inconsistent behaviour. For example, a non-homogeneous
    /// `Vec<Value>` is not a valid Solidity type (which is why the [`Array`]
    /// abstraction exists).
    pub fn encode(&self) -> Vec<u8> {
        crate::encode(&Encodable(self))
    }

    /// Encodes a `Value` with a function selector.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_with_selector(&self, selector: Selector) -> Vec<u8> {
        crate::encode_with_selector(selector, &Encodable(self))
    }

    /// Encodes a `Value` with a prefix.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_with_prefix(&self, prefix: &[u8]) -> Vec<u8> {
        crate::encode_with_prefix(prefix, &Encodable(self))
    }

    /// Encodes a `Value` to the specified buffer.
    ///
    /// See [`Value::encode`] for more details.
    pub fn encode_to(&self, buffer: &mut [u8]) -> Result<(), BufferSizeError> {
        crate::encode_to(buffer, &Encodable(self))
    }

    /// Decodes a `Value`.
    ///
    /// Note that `Value`s can't use the [`crate::decode()`] method directly as
    /// it requires runtime type information for proper decoding.
    pub fn decode(kind: &ValueKind, bytes: &[u8]) -> Result<Self, DecodeError> {
        Ok(context::decode::<Decodable>(bytes, kind)?.0)
    }

    /// Decodes a `Value` from data prefixed with a selector.
    ///
    /// See [`Value::decode`] for more details.
    pub fn decode_with_selector(
        kind: &ValueKind,
        selector: Selector,
        bytes: &[u8],
    ) -> Result<Self, DecodeError> {
        Ok(context::decode_with_selector::<Decodable>(selector, bytes, kind)?.0)
    }

    /// Decodes a `Value` from data with an expected byte prefix.
    ///
    /// See [`Value::decode`] for more details.
    pub fn decode_with_prefix(
        kind: &ValueKind,
        prefix: &[u8],
        bytes: &[u8],
    ) -> Result<Self, DecodeError> {
        Ok(context::decode_with_prefix::<Decodable>(prefix, bytes, kind)?.0)
    }
}

/// Internal type for implementing encoding on [`Value`]s.
struct Encodable<'a>(&'a Value);

impl Encode for Encodable<'_> {
    fn size(&self) -> Size {
        match self.0 {
            Value::Int(value) => value.1.size(),
            Value::Uint(value) => value.1.size(),
            Value::Address(value) => value.size(),
            Value::Bool(value) => value.size(),
            Value::FixedBytes(value) => value.1.size(),
            Value::Function(value) => value.size(),
            Value::Bytes(value) => Bytes::borrowed(value).size(),
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
            Value::Int(value) => value.1.encode(encoder),
            Value::Uint(value) => value.1.encode(encoder),
            Value::Address(value) => value.encode(encoder),
            Value::Bool(value) => value.encode(encoder),
            Value::FixedBytes(value) => value.1.encode(encoder),
            Value::Function(value) => value.encode(encoder),
            Value::Bytes(value) => Bytes::borrowed(value).encode(encoder),
            Value::String(value) => value.encode(encoder),
            Value::Array(Array(_, values)) => {
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

/// Internal type for implementing decoding on [`Value`]s.
struct Decodable(Value);

impl DecodeContext for Decodable {
    type Context = ValueKind;

    fn is_dynamic_context(context: &Self::Context) -> bool {
        match context {
            ValueKind::Int(_)
            | ValueKind::Uint(_)
            | ValueKind::Address
            | ValueKind::Bool
            | ValueKind::FixedBytes(_)
            | ValueKind::Function => false,
            ValueKind::FixedArray(_, element) => Self::is_dynamic_context(element),
            ValueKind::Bytes | ValueKind::String | ValueKind::Array(_) => true,
            ValueKind::Tuple(elements) => elements.iter().any(Self::is_dynamic_context),
        }
    }

    fn decode_context(decoder: &mut Decoder, context: &Self::Context) -> Result<Self, DecodeError> {
        let value = match context {
            ValueKind::Int(bit_width) => Value::Int(Int(*bit_width, decoder.read()?)),
            ValueKind::Uint(bit_width) => Value::Uint(Uint(*bit_width, decoder.read()?)),
            ValueKind::Address => Value::Address(decoder.read()?),
            ValueKind::Bool => Value::Bool(decoder.read()?),
            ValueKind::FixedBytes(byte_length) => {
                Value::FixedBytes(FixedBytes(*byte_length, decoder.read()?))
            }
            ValueKind::Function => Value::Function(decoder.read()?),
            ValueKind::FixedArray(len, element) => Value::FixedArray(Array(
                (**element).clone(),
                (0..*len)
                    .map(|_| Ok(decoder.read_context::<Decodable>(element)?.0))
                    .collect::<Result<_, _>>()?,
            )),
            ValueKind::Bytes => Value::Bytes(Bytes::decode(decoder)?.0),
            ValueKind::String => Value::String(String::decode(decoder)?),
            ValueKind::Array(element) => {
                let len = decoder.read_size()?;
                let mut decoder = decoder.anchor();
                Value::Array(Array(
                    (**element).clone(),
                    (0..len)
                        .map(|_| Ok(decoder.read_context::<Decodable>(element)?.0))
                        .collect::<Result<_, _>>()?,
                ))
            }
            ValueKind::Tuple(elements) => Value::Tuple(
                elements
                    .iter()
                    .map(|element| Ok(decoder.read_context::<Decodable>(element)?.0))
                    .collect::<Result<_, _>>()?,
            ),
        };

        Ok(Self(value))
    }
}

/// The kind of the Solidity value.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueKind {
    /// The Solidity `intNN` type.
    Int(BitWidth),
    /// The Solidity `uintNN` type.
    Uint(BitWidth),
    /// The Solidity `address` type.
    Address,
    /// The Solidity `bool` type.
    Bool,
    /// The Solidity `bytesNN` type.
    FixedBytes(ByteLength),
    /// The Solidity `function (...) external ...` type.
    Function,
    /// The Solidity `...[NN]` type.
    FixedArray(usize, Box<ValueKind>),
    /// The Solidity `bytes` type.
    Bytes,
    /// The Solidity `string` type.
    String,
    /// The Solidity `...[]` type.
    Array(Box<ValueKind>),
    /// The Solidity `(...)` type.
    Tuple(Vec<ValueKind>),
}

impl ValueKind {
    /// The `uint` value kind, which is equivalent to a `uint256`.
    pub const UINT: Self = Self::Uint(BitWidth::MAX);

    /// The `int` value kind, which is equivalent to a `int256`.
    pub const INT: Self = Self::Int(BitWidth::MAX);

    /// The value kind used for enums.
    pub const ENUM: Self = Self::Uint(BitWidth::ENUM);

    /// Returns true if the value kind is a primitive type.
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Self::Int(_)
                | Self::Uint(_)
                | Self::Address
                | Self::Bool
                | Self::FixedBytes(_)
                | Self::Function
        )
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ValueKind::Int(bit_width) => write!(f, "int{}", bit_width.0),
            ValueKind::Uint(bit_width) => write!(f, "uint{}", bit_width.0),
            ValueKind::Address => f.write_str("address"),
            ValueKind::Bool => f.write_str("bool"),
            ValueKind::FixedBytes(len) => write!(f, "bytes{}", len.0),
            ValueKind::Function => f.write_str("function"),
            ValueKind::FixedArray(len, kind) => write!(f, "{kind}[{len}]"),
            ValueKind::Bytes => f.write_str("bytes"),
            ValueKind::String => f.write_str("string"),
            ValueKind::Array(kind) => write!(f, "{kind}[]"),
            ValueKind::Tuple(fields) => {
                let mut fields = fields.iter();
                f.write_str("(")?;
                if let Some(kind) = fields.next() {
                    write!(f, "{kind}")?;
                }
                for kind in fields {
                    write!(f, ",{kind}")?;
                }
                f.write_str(")")
            }
        }
    }
}

impl FromStr for ValueKind {
    type Err = ParseValueKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => return Ok(Self::Int(BitWidth::default())),
            "uint" => return Ok(Self::Uint(BitWidth::default())),
            "address" => return Ok(Self::Address),
            "bool" => return Ok(Self::Bool),
            "function" => return Ok(Self::Function),
            "bytes" => return Ok(Self::Bytes),
            "string" => return Ok(Self::String),
            _ => {}
        }

        if let Some(prefix) = s.strip_suffix("[]") {
            return Ok(Self::Array(Box::new(prefix.parse()?)));
        }
        if let Some((prefix, n)) = s.strip_suffix(']').and_then(|s| {
            let (prefix, n) = s.rsplit_once('[')?;
            Some((prefix, n.parse::<usize>().ok()?))
        }) {
            return Ok(Self::FixedArray(n, Box::new(prefix.parse()?)));
        }

        if let Some(n) = s.strip_prefix("int") {
            let bit_width = n
                .parse()
                .ok()
                .and_then(BitWidth::new)
                .ok_or(ParseValueKindError::InvalidBitWidth)?;
            return Ok(Self::Int(bit_width));
        }
        if let Some(n) = s.strip_prefix("uint") {
            let bit_width = n
                .parse()
                .ok()
                .and_then(BitWidth::new)
                .ok_or(ParseValueKindError::InvalidBitWidth)?;
            return Ok(Self::Uint(bit_width));
        }
        if let Some(n) = s.strip_prefix("bytes") {
            let len = n
                .parse()
                .ok()
                .and_then(ByteLength::new)
                .ok_or(ParseValueKindError::InvalidByteLength)?;
            return Ok(Self::FixedBytes(len));
        }

        if let Some(mut ss) = s.strip_prefix('(').and_then(|s| s.strip_suffix(')')) {
            let mut fields = Vec::<ValueKind>::new();
            while let Some((s, rest)) = ss.split_once(',') {
                fields.push(s.parse()?);
                ss = rest;
            }
            return Ok(ValueKind::Tuple(fields));
        }

        Err(ParseValueKindError::Unknown)
    }
}

/// An error parsing value kinds.
pub enum ParseValueKindError {
    InvalidBitWidth,
    InvalidByteLength,
    FunctionSignature,
    Unknown,
}

/// An intenger bit width.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BitWidth(u32);

impl BitWidth {
    /// The smallest possible bit-width.
    pub const MIN: Self = Self(8);

    /// The largets possible bit-width.
    pub const MAX: Self = Self(256);

    /// The bit-width used for encoding `enum` values.
    pub const ENUM: Self = Self(8);

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

    /// Returns the bit width as a [`u32`].
    pub fn get(self) -> u32 {
        self.0
    }
}

impl Default for BitWidth {
    fn default() -> Self {
        Self(256)
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
    pub fn new(bit_width: u32, value: I256) -> Option<Self> {
        let bit_width = BitWidth::new(bit_width)?;
        let value = bit_width.resize(value)?;
        Some(Self(bit_width, value))
    }

    /// Returns the integer's bit width.
    pub fn bit_width(&self) -> BitWidth {
        self.0
    }

    /// Returns the integer as a [`I256`]. The integer will be sign extended if
    /// its bit width is less than 256.
    pub fn get(&self) -> I256 {
        self.1
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
    pub fn new(bit_width: u32, value: U256) -> Option<Self> {
        let bit_width = BitWidth::new(bit_width)?;
        let value = bit_width.resize(value)?;
        Some(Self(bit_width, value))
    }

    /// Returns the integer's bit width.
    pub fn bit_width(&self) -> BitWidth {
        self.0
    }

    /// Returns the integer as a [`U256`].
    pub fn get(&self) -> U256 {
        self.1
    }
}

/// A fixes byte array length.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ByteLength(usize);

impl ByteLength {
    /// The smallest possible byte-length.
    pub const MIN: Self = Self(1);

    /// The largets possible byte-length.
    pub const MAX: Self = Self(32);

    /// The byte-length used for encoding selectors.
    pub const SELECTOR: Self = Self(4);

    /// The byte-length used for encoding hashes.
    pub const HASH: Self = Self(32);

    /// Checked bit-width creation.
    ///
    /// Bitwidths must be multiple of 8s between 8 and 256.
    pub fn new(len: usize) -> Option<Self> {
        match len {
            1..=32 => Some(Self(len)),
            _ => None,
        }
    }

    /// Returns the byte length as a [`usize`].
    pub fn get(self) -> usize {
        self.0
    }
}

/// A dynamic value for representing Solidity fixed bytes type.
///
/// This is different than using [`crate::bytes::Bytes`] in that the size is not
/// known at compile time.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FixedBytes(ByteLength, [u8; 32]);

impl FixedBytes {
    /// Creates a new fixed byte value.
    pub fn new(bytes: &[u8]) -> Option<Self> {
        let len = ByteLength::new(bytes.len())?;
        let mut word = Word::default();
        word[..len.0].copy_from_slice(bytes);
        Some(Self(len, word))
    }

    /// Returns the byte length of the fixed bytes.
    pub fn byte_length(&self) -> ByteLength {
        self.0
    }

    /// Returns the fixed byte value as a slice of bytes.
    pub fn as_bytes(&self) -> &[u8] {
        &self.1[..self.0.get()]
    }

    /// Returns the fixed bytes as a word.
    pub fn into_word(self) -> Word {
        self.1
    }
}

macro_rules! impl_from_byte_array_for_fixed_bytes {
    ($($n:literal,)*) => {$(
        impl From<[u8; $n]> for FixedBytes {
            fn from(value: [u8; $n]) -> Self {
                let mut bytes = [0; 32];
                bytes[..$n].copy_from_slice(&value);
                Self(ByteLength($n), bytes)
            }
        }
    )*};
}

impl_from_byte_array_for_fixed_bytes! {
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
}

/// An array of values.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Array(ValueKind, Vec<Value>);

impl Array {
    /// Creates a new array value with the specified element kind.
    pub fn new(element_kind: ValueKind, values: Vec<Value>) -> Option<Self> {
        if values.iter().any(|value| value.kind() != element_kind) {
            return None;
        }
        Some(Self(element_kind, values))
    }

    /// Creates a new array value from a vector of values.
    pub fn from_values(values: Vec<Value>) -> Option<Self> {
        let element_kind = values.first()?.kind();
        Self::new(element_kind, values)
    }

    /// Creates an empty array.
    pub fn empty(element_kind: ValueKind) -> Self {
        Self(element_kind, Vec::new())
    }

    /// Returns the value kind of the array element.
    pub fn element_kind(&self) -> &ValueKind {
        &self.0
    }

    /// Returns the array as a slice.
    pub fn as_slice(&self) -> &[Value] {
        &self.1
    }

    /// Returns the length of the array.
    pub fn len(&self) -> usize {
        self.1.len()
    }

    /// Returns whether or not the array is empty.
    pub fn is_empty(&self) -> bool {
        self.1.is_empty()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_bytes_kind() {
        assert_eq!(
            Value::FixedBytes(FixedBytes::new(&[1]).unwrap()).kind(),
            ValueKind::FixedBytes(ByteLength::new(1).unwrap()),
        );
    }
}
