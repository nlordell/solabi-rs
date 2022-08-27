//! Raw JSON ABI types.
//!
//! We convert `Fragments` this intermediate representation for serializing
//! and deserializing to JSON in order to be able to deal with different ABI
//! outputs from different compiler versions.

use crate::value::ValueKind;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter, Write as _},
    str::FromStr,
};

/// The JSON descriptor.
#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Descriptor {
    /// The kind of the descriptor.
    #[serde(rename = "type")]
    pub kind: DescriptorKind,
    /// The name.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Inputs to the ABI item.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inputs: Option<Vec<Field>>,
    /// Outputs of the ABI item.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outputs: Option<Vec<Field>>,
    /// The state mutability for the specified item.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state_mutability: Option<StateMutability>,
    /// Whether or not the item modifies state.
    ///
    /// This was provided by older compiler versions and was deprecated in
    /// favour of `state_mutability`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub constant: Option<bool>,
    /// Whether or not the item is payable.
    ///
    /// This was provided by older compiler versions and was deprecated in
    /// favour of `state_mutability`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub payable: Option<bool>,
    /// Whether or not an event is anonymous.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anonymous: Option<bool>,
}

impl Descriptor {
    /// Retrieves the state mutablility, falling back to constant and payable
    /// flags if not explicitely set.
    pub fn state_mutability(&self) -> StateMutability {
        match (self.state_mutability, self.constant, self.payable) {
            (Some(state_mutability), _, _) => state_mutability,
            (None, Some(constant), Some(payable)) => StateMutability::with_flags(constant, payable),
            _ => StateMutability::Payable,
        }
    }
}

/// The kind of descriptor.
#[derive(Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum DescriptorKind {
    /// A function.
    Function,
    /// A constructor.
    Constructor,
    /// A "receive Ether" function.
    Receive,
    /// A fallback function.
    Fallback,
    /// An event.
    Event,
    /// A constructor.
    Error,
}

impl From<DescriptorKind> for Descriptor {
    fn from(kind: DescriptorKind) -> Self {
        Self {
            kind,
            name: None,
            inputs: None,
            outputs: None,
            state_mutability: None,
            constant: None,
            payable: None,
            anonymous: None,
        }
    }
}

/// A field.
#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Field {
    /// The name.
    pub name: String,
    /// The field type.
    #[serde(rename = "type")]
    pub kind: FieldKind,
    /// The field components in case of a tuple.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Vec<Field>>,
    /// Wether or not the field is indexed for events.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indexed: Option<bool>,
    /// The descriptive internal type name.
    pub internal_type: Option<String>,
}

impl Field {
    /// Fixes a field's type.
    ///
    /// This is for dealing with a very very very special case where:
    /// - `tuple` is a valid name for an `enum`
    /// - dealing with a library type, so the `type` will be the incredibly
    ///   confusing enum name `tuple`.
    ///
    /// ```text
    /// enum tuple { X }
    /// library L {
    ///     function f(tuple x) public pure { }
    /// }
    /// ```
    ///
    /// This will produce the following field:
    /// ```text
    /// {
    ///     "internalType": "enum tuple",
    ///     "name": "x",
    ///     "type": "tuple"
    /// }
    /// ```
    pub fn fixup(mut self) -> Self {
        fn fix(f: &mut Field) {
            if matches!(
                &f.kind,
                FieldKind::Tuple(TupleKind::Single)
                    if matches!(&f.internal_type, Some(ref t) if t.starts_with("enum ")),
            ) {
                f.kind = FieldKind::Other("tuple".to_string());
            }

            if let Some(components) = &mut f.components {
                for field in components {
                    fix(field);
                }
            }
        }

        fix(&mut self);
        self
    }

    /// Returns the alternate library function paramter type name used for
    /// selector computation, if there is one.
    ///
    /// Library ABIs are weird in that `enum`s are represented as their name
    /// and not their underlying type (`uint8`). Furthermore, to complicate
    /// things, `tuple` is a valid enum type name!
    pub fn library_parameter_type(&self) -> Option<String> {
        if !self.is_library_enum_type() {
            return None;
        }

        let mut buffer = String::new();
        self.write_type(&mut buffer).unwrap();
        Some(buffer)
    }

    /// Returns true if the field is a library enum type.
    fn is_library_enum_type(&self) -> bool {
        match &self.kind {
            FieldKind::Value(_) => false,
            FieldKind::Tuple(_) => self
                .components
                .iter()
                .flatten()
                .any(Field::is_library_enum_type),
            FieldKind::Other(_) => true,
        }
    }

    /// Writes the field's parameter type to the buffer.
    fn write_type(&self, buffer: &mut String) -> fmt::Result {
        match &self.kind {
            FieldKind::Value(kind) => write!(buffer, "{kind}"),
            FieldKind::Tuple(kind) => {
                write!(buffer, "(")?;
                for (i, field) in self.components.iter().flatten().enumerate() {
                    if i != 0 {
                        write!(buffer, ",")?;
                    }
                    field.write_type(buffer)?;
                }
                write!(buffer, "){}", kind.suffix())
            }
            FieldKind::Other(name) => write!(buffer, "{name}"),
        }
    }
}

/// A field canonical type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FieldKind {
    /// An ABI type.
    Value(ValueKind),
    /// A tuple type, inner field types included in the adjacent `components`.
    Tuple(TupleKind),
    /// Another type.
    ///
    /// This is required to deal with `library` ABIs which output different
    /// values for enumerations.
    Other(String),
}

/// A tuple specification.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TupleKind {
    /// The tuple itself.
    Single,
    /// A fixed size array of a tuple type.
    FixedArray(usize, Box<TupleKind>),
    /// A dynamically sized array of a tuple type.
    Array(Box<TupleKind>),
}

impl TupleKind {
    /// Converts a value kind to a tuple kind and returns the tuple fields.
    ///
    /// Returns `None` if the value isn't a tuple type.
    pub fn from_value(kind: &ValueKind) -> Option<(Self, &[ValueKind])> {
        match kind {
            ValueKind::FixedArray(n, kind) => {
                let (kind, fields) = Self::from_value(kind)?;
                Some((Self::FixedArray(*n, Box::new(kind)), fields))
            }
            ValueKind::Array(kind) => {
                let (kind, fields) = Self::from_value(kind)?;
                Some((Self::Array(Box::new(kind)), fields))
            }
            ValueKind::Tuple(fields) => Some((Self::Single, fields)),
            _ => None,
        }
    }

    /// Converts a tuple kind to a value kind given its field types.
    pub fn to_value(&self, components: Vec<ValueKind>) -> ValueKind {
        match self {
            TupleKind::Single => ValueKind::Tuple(components),
            TupleKind::FixedArray(n, kind) => {
                ValueKind::FixedArray(*n, Box::new(kind.to_value(components)))
            }
            TupleKind::Array(kind) => ValueKind::Array(Box::new(kind.to_value(components))),
        }
    }

    /// Returns a display for the tuple-kind array suffix.
    fn suffix(&self) -> TupleSuffix<'_> {
        TupleSuffix(self)
    }
}

/// A display implementation for the tuple kind's array suffix.
struct TupleSuffix<'a>(&'a TupleKind);

impl Display for TupleKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("tuple")?;
        write!(f, "{}", self.suffix())
    }
}

impl Display for TupleSuffix<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0 {
            TupleKind::Single => Ok(()),
            TupleKind::FixedArray(n, kind) => write!(f, "{}[{n}]", kind.suffix()),
            TupleKind::Array(kind) => write!(f, "{}[]", kind.suffix()),
        }
    }
}

/// An error parsing tuple kinds.
pub struct ParseTupleKindError;

impl FromStr for TupleKind {
    type Err = ParseTupleKindError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "tuple" {
            Ok(Self::Single)
        } else if let Some(prefix) = s.strip_suffix("[]") {
            Ok(Self::Array(Box::new(prefix.parse()?)))
        } else if let Some((prefix, n)) = s.strip_suffix(']').and_then(|s| {
            let (prefix, n) = s.rsplit_once('[')?;
            Some((prefix, n.parse::<usize>().ok()?))
        }) {
            Ok(Self::FixedArray(n, Box::new(prefix.parse()?)))
        } else {
            Err(ParseTupleKindError)
        }
    }
}

impl<'de> Deserialize<'de> for FieldKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let name = Cow::<str>::deserialize(deserializer)?;
        if name.starts_with("tuple") {
            if let Ok(kind) = name.parse() {
                return Ok(FieldKind::Tuple(kind));
            }
        }
        if let Ok(kind) = name.parse() {
            return Ok(FieldKind::Value(kind));
        }
        Ok(FieldKind::Other(name.into_owned()))
    }
}

impl Serialize for FieldKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            FieldKind::Value(kind) => match TupleKind::from_value(kind) {
                // Kind of abiguous here... but in case we have a `ValueKind`
                // that is actually a tuple, convert it to its tuple kind first.
                Some((tuple, _)) => serializer.serialize_str(&tuple.to_string()),
                None => serializer.serialize_str(&kind.to_string()),
            },
            FieldKind::Tuple(kind) => serializer.serialize_str(&kind.to_string()),
            FieldKind::Other(value) => serializer.serialize_str(value),
        }
    }
}

/// Code execution state mutability.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum StateMutability {
    /// Function that executes without reading blockchain state.
    Pure,
    /// Function that reads, but does not modify, blockchain state.
    View,
    /// Function that potentially modifies blockchain state, but cannot receive
    /// any Ether value.
    NonPayable,
    /// Function that potentially modifies blockchain state, and can receive
    /// Ether value.
    Payable,
}

impl StateMutability {
    /// Returns the state mutability given constant and payable flags.
    pub fn with_flags(constant: bool, payable: bool) -> StateMutability {
        match (constant, payable) {
            (_, true) => Self::Payable,
            (true, false) => Self::NonPayable,
            (false, false) => Self::View,
        }
    }
}
