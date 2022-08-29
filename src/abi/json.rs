//! Raw JSON ABI serialization implementation.
//!
//! We convert `abi::Descriptor`s to and from this intermediate representation
//! for serializing and deserializing to JSON in order to be able to deal with
//! different ABI outputs from different compiler versions.

use crate::{
    abi::{self, StateMutability},
    value::ValueKind,
};
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
            (None, constant, payable) => StateMutability::with_flags(
                constant.unwrap_or_default(),
                payable.unwrap_or_default(),
            ),
        }
    }
}

macro_rules! check {
    ($cond:expr, $err:literal) => {
        if !($cond) {
            return Err($err);
        }
    };
}

impl TryFrom<Descriptor> for abi::Descriptor {
    type Error = &'static str;

    fn try_from(
        value: Descriptor,
    ) -> Result<Self, <abi::Descriptor as TryFrom<Descriptor>>::Error> {
        match &value.kind {
            DescriptorKind::Function => value.try_into().map(Self::Function),
            DescriptorKind::Constructor => value.try_into().map(Self::Constructor),
            DescriptorKind::Receive => Ok(Self::Receive),
            DescriptorKind::Fallback => Ok(Self::Fallback(value.state_mutability())),
            DescriptorKind::Event => value.try_into().map(Self::Event),
            DescriptorKind::Error => value.try_into().map(Self::Error),
        }
    }
}

impl From<abi::Descriptor> for Descriptor {
    fn from(descriptor: abi::Descriptor) -> Self {
        match descriptor {
            abi::Descriptor::Function(function) => function.into(),
            abi::Descriptor::Constructor(constructor) => constructor.into(),
            abi::Descriptor::Receive => Self {
                state_mutability: Some(StateMutability::Payable),
                ..DescriptorKind::Receive.into()
            },
            abi::Descriptor::Fallback(state_mutability) => Self {
                state_mutability: Some(state_mutability),
                ..DescriptorKind::Fallback.into()
            },
            abi::Descriptor::Event(event) => event.into(),
            abi::Descriptor::Error(error) => error.into(),
        }
    }
}

impl TryFrom<Descriptor> for abi::FunctionDescriptor {
    type Error = &'static str;

    fn try_from(value: Descriptor) -> Result<Self, Self::Error> {
        check!(value.kind == DescriptorKind::Function, "not a function");
        let state_mutability = value.state_mutability();
        Ok(Self {
            name: value.name.ok_or("function missing name")?,
            inputs: from_fields(value.inputs.ok_or("function missing inputs")?)?,
            outputs: from_fields(value.outputs.ok_or("function missing outputs")?)?,
            state_mutability,
        })
    }
}

impl From<abi::FunctionDescriptor> for Descriptor {
    fn from(function: abi::FunctionDescriptor) -> Self {
        Self {
            name: Some(function.name),
            inputs: Some(to_fields(function.inputs)),
            outputs: Some(to_fields(function.outputs)),
            state_mutability: Some(function.state_mutability),
            ..DescriptorKind::Function.into()
        }
    }
}

impl TryFrom<Descriptor> for abi::ConstructorDescriptor {
    type Error = &'static str;

    fn try_from(value: Descriptor) -> Result<Self, Self::Error> {
        check!(
            value.kind == DescriptorKind::Constructor,
            "not a constructor"
        );
        let state_mutability = value.state_mutability();
        Ok(Self {
            inputs: from_fields(value.inputs.ok_or("constructor missing inputs")?)?,
            state_mutability,
        })
    }
}

impl From<abi::ConstructorDescriptor> for Descriptor {
    fn from(constructor: abi::ConstructorDescriptor) -> Self {
        Self {
            inputs: Some(to_fields(constructor.inputs)),
            state_mutability: Some(constructor.state_mutability),
            ..DescriptorKind::Constructor.into()
        }
    }
}

impl TryFrom<Descriptor> for abi::EventDescriptor {
    type Error = &'static str;

    fn try_from(value: Descriptor) -> Result<Self, Self::Error> {
        check!(value.kind == DescriptorKind::Event, "not an event");
        Ok(Self {
            name: value.name.ok_or("event missing name")?,
            inputs: from_fields(value.inputs.ok_or("event missing inputs")?)?,
            anonymous: value.anonymous.unwrap_or_default(),
        })
    }
}

impl From<abi::EventDescriptor> for Descriptor {
    fn from(event: abi::EventDescriptor) -> Self {
        Self {
            name: Some(event.name),
            inputs: Some(to_fields(event.inputs)),
            anonymous: Some(event.anonymous),
            ..DescriptorKind::Event.into()
        }
    }
}

impl TryFrom<Descriptor> for abi::ErrorDescriptor {
    type Error = &'static str;

    fn try_from(value: Descriptor) -> Result<Self, Self::Error> {
        check!(value.kind == DescriptorKind::Error, "not an error");
        Ok(Self {
            name: value.name.ok_or("error missing name")?,
            inputs: from_fields(value.inputs.ok_or("error missing inputs")?)?,
        })
    }
}

impl From<abi::ErrorDescriptor> for Descriptor {
    fn from(error: abi::ErrorDescriptor) -> Self {
        Self {
            name: Some(error.name),
            inputs: Some(to_fields(error.inputs)),
            ..DescriptorKind::Error.into()
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
    #[serde(default)]
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

impl TryFrom<Field> for abi::Field {
    type Error = &'static str;

    fn try_from(value: Field) -> Result<Self, Self::Error> {
        let kind = (&value).try_into()?;
        Ok(Self {
            name: value.name,
            kind,
            components: value
                .components
                .map(|components| {
                    components
                        .into_iter()
                        .map(abi::Field::try_from)
                        .collect::<Result<_, _>>()
                })
                .transpose()?,
            internal_type: value.internal_type,
        })
    }
}

impl TryFrom<&'_ Field> for ValueKind {
    type Error = &'static str;

    /// Converts a tuple kind to its value kind.
    fn try_from(value: &'_ Field) -> Result<Self, Self::Error> {
        match &value.kind {
            FieldKind::Value(kind) => Ok(kind.clone()),
            FieldKind::Tuple(kind) => {
                let components = value
                    .components
                    .as_ref()
                    .ok_or("missing components for tuple field")?
                    .iter()
                    .map(ValueKind::try_from)
                    .collect::<Result<_, _>>()?;
                Ok(kind.to_value(components))
            }
            FieldKind::Other(_) => Ok(ValueKind::ENUM),
        }
    }
}

impl From<abi::Field> for Field {
    fn from(field: abi::Field) -> Self {
        let (kind, components) = match TupleKind::from_value(&field.kind) {
            Some((kind, fields)) => {
                let components = match field.components {
                    Some(components) => components.into_iter().map(Field::from).collect(),
                    None => fields.iter().map(Field::from).collect(),
                };
                (FieldKind::Tuple(kind), Some(components))
            }
            None => (FieldKind::Value(field.kind), None),
        };
        Self {
            name: field.name,
            kind,
            components,
            indexed: None,
            internal_type: field.internal_type,
        }
    }
}

impl From<&'_ ValueKind> for Field {
    fn from(kind: &ValueKind) -> Self {
        let (kind, components) = match TupleKind::from_value(kind) {
            Some((kind, fields)) => (
                FieldKind::Tuple(kind),
                Some(fields.iter().map(Field::from).collect()),
            ),
            None => (FieldKind::Value(kind.clone()), None),
        };
        Self {
            name: String::new(),
            kind,
            components,
            indexed: None,
            internal_type: None,
        }
    }
}

impl TryFrom<Field> for abi::Parameter {
    type Error = &'static str;

    fn try_from(value: Field) -> Result<Self, Self::Error> {
        let value = value.fixup();
        let kind_name = value.library_parameter_type();
        Ok(Self {
            field: value.try_into()?,
            kind_name,
        })
    }
}

impl From<abi::Parameter> for Field {
    fn from(parameter: abi::Parameter) -> Self {
        let mut field = Field::from(parameter.field);
        if let Some(kind_name) = parameter.kind_name {
            field.kind = FieldKind::Other(kind_name);
        }
        field
    }
}

impl TryFrom<Field> for abi::EventField {
    type Error = &'static str;

    fn try_from(value: Field) -> Result<Self, Self::Error> {
        let indexed = value.indexed.unwrap_or_default();
        Ok(Self {
            field: value.try_into()?,
            indexed,
        })
    }
}

impl From<abi::EventField> for Field {
    fn from(field: abi::EventField) -> Self {
        Self {
            indexed: Some(field.indexed),
            ..field.field.into()
        }
    }
}

fn from_fields<T>(fields: Vec<Field>) -> Result<Vec<T>, T::Error>
where
    T: TryFrom<Field>,
{
    fields.into_iter().map(T::try_from).collect()
}

fn to_fields<T>(fields: Vec<T>) -> Vec<Field>
where
    T: Into<Field>,
{
    fields.into_iter().map(T::into).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{abi::Abi, value::BitWidth};
    use serde_json::json;

    #[test]
    fn ethers_js_example_json_abi() {
        let descriptors = serde_json::from_str::<Vec<Descriptor>>(
            r#"
            [
              {
                "type": "constructor",
                "payable": false,
                "inputs": [
                  { "type": "string", "name": "symbol" },
                  { "type": "string", "name": "name" }
                ]
              },
              {
                "type": "function",
                "name": "transferFrom",
                "constant": false,
                "payable": false,
                "inputs": [
                  { "type": "address", "name": "from" },
                  { "type": "address", "name": "to" },
                  { "type": "uint256", "name": "value" }
                ],
                "outputs": [ ]
              },
              {
                "type": "function",
                "name": "balanceOf",
                "constant":true,
                "stateMutability": "view",
                "payable":false, "inputs": [
                  { "type": "address", "name": "owner" }
                ],
                "outputs": [
                  { "type": "uint256" }
                ]
              },
              {
                "type": "event",
                "anonymous": false,
                "name": "Transfer",
                "inputs": [
                  { "type": "address", "name": "from", "indexed":true },
                  { "type": "address", "name": "to", "indexed":true },
                  { "type": "uint256", "name": "value" }
                ]
              },
              {
                "type": "error",
                "name": "InsufficientBalance",
                "inputs": [
                  { "type": "address", "name": "owner" },
                  { "type": "uint256", "name": "balance" }
                ]
              },
              {
                "type": "function",
                "name": "addPerson",
                "constant": false,
                "payable": false,
                "inputs": [
                  {
                    "type": "tuple",
                    "name": "person",
                    "components": [
                      { "type": "string", "name": "name" },
                      { "type": "uint16", "name": "age" }
                    ]
                  }
                ],
                "outputs": []
              },
              {
                "type": "function",
                "name": "addPeople",
                "constant": false,
                "payable": false,
                "inputs": [
                  {
                    "type": "tuple[]",
                    "name": "person",
                    "components": [
                      { "type": "string", "name": "name" },
                      { "type": "uint16", "name": "age" }
                    ]
                  }
                ],
                "outputs": []
              },
              {
                "type": "function",
                "name": "getPerson",
                "constant": true,
                "stateMutability": "view",
                "payable": false,
                "inputs": [
                  { "type": "uint256", "name": "id" }
                ],
                "outputs": [
                  {
                    "type": "tuple",
                    "components": [
                      { "type": "string", "name": "name" },
                      { "type": "uint16", "name": "age" }
                    ]
                  }
                ]
              },
              {
                "type": "event",
                "anonymous": false,
                "name": "PersonAdded",
                "inputs": [
                  { "type": "uint256", "name": "id", "indexed": true },
                  {
                    "type": "tuple",
                    "name": "person",
                    "components": [
                      { "type": "string", "name": "name", "indexed": false },
                      { "type": "uint16", "name": "age", "indexed": false }
                    ]
                  }
                ]
              }
            ]
        "#,
        )
        .unwrap();

        let abi = descriptors
            .into_iter()
            .map(|d| abi::Descriptor::try_from(d).unwrap())
            .collect::<Vec<_>>();

        let expected = vec![
            abi::Descriptor::Constructor(abi::ConstructorDescriptor {
                inputs: vec![
                    abi::Parameter {
                        field: abi::Field {
                            name: "symbol".to_owned(),
                            kind: ValueKind::String,
                            components: None,
                            internal_type: None,
                        },
                        kind_name: None,
                    },
                    abi::Parameter {
                        field: abi::Field {
                            name: "name".to_owned(),
                            kind: ValueKind::String,
                            components: None,
                            internal_type: None,
                        },
                        kind_name: None,
                    },
                ],
                state_mutability: StateMutability::NonPayable,
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "transferFrom".to_owned(),
                inputs: vec![
                    abi::Parameter {
                        field: abi::Field {
                            name: "from".to_owned(),
                            kind: ValueKind::Address,
                            components: None,
                            internal_type: None,
                        },
                        kind_name: None,
                    },
                    abi::Parameter {
                        field: abi::Field {
                            name: "to".to_owned(),
                            kind: ValueKind::Address,
                            components: None,
                            internal_type: None,
                        },
                        kind_name: None,
                    },
                    abi::Parameter {
                        field: abi::Field {
                            name: "value".to_owned(),
                            kind: ValueKind::UINT,
                            components: None,
                            internal_type: None,
                        },
                        kind_name: None,
                    },
                ],
                outputs: vec![],
                state_mutability: StateMutability::NonPayable,
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "balanceOf".to_owned(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "owner".to_owned(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                outputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "".to_owned(),
                        kind: ValueKind::UINT,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                state_mutability: StateMutability::View,
            }),
            abi::Descriptor::Event(abi::EventDescriptor {
                name: "Transfer".to_owned(),
                inputs: vec![
                    abi::EventField {
                        field: abi::Field {
                            name: "from".to_owned(),
                            kind: ValueKind::Address,
                            components: None,
                            internal_type: None,
                        },
                        indexed: true,
                    },
                    abi::EventField {
                        field: abi::Field {
                            name: "to".to_owned(),
                            kind: ValueKind::Address,
                            components: None,
                            internal_type: None,
                        },
                        indexed: true,
                    },
                    abi::EventField {
                        field: abi::Field {
                            name: "value".to_owned(),
                            kind: ValueKind::UINT,
                            components: None,
                            internal_type: None,
                        },
                        indexed: false,
                    },
                ],
                anonymous: false,
            }),
            abi::Descriptor::Error(abi::ErrorDescriptor {
                name: "InsufficientBalance".to_owned(),
                inputs: vec![
                    abi::Field {
                        name: "owner".to_owned(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    abi::Field {
                        name: "balance".to_owned(),
                        kind: ValueKind::UINT,
                        components: None,
                        internal_type: None,
                    },
                ],
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "addPerson".to_owned(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "person".to_owned(),
                        kind: ValueKind::Tuple(vec![
                            ValueKind::String,
                            ValueKind::Uint(BitWidth::new(16).unwrap()),
                        ]),
                        components: Some(vec![
                            abi::Field {
                                name: "name".to_owned(),
                                kind: ValueKind::String,
                                components: None,
                                internal_type: None,
                            },
                            abi::Field {
                                name: "age".to_owned(),
                                kind: ValueKind::Uint(BitWidth::new(16).unwrap()),
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                outputs: vec![],
                state_mutability: StateMutability::NonPayable,
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "addPeople".to_owned(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "person".to_owned(),
                        kind: ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                            ValueKind::String,
                            ValueKind::Uint(BitWidth::new(16).unwrap()),
                        ]))),
                        components: Some(vec![
                            abi::Field {
                                name: "name".to_owned(),
                                kind: ValueKind::String,
                                components: None,
                                internal_type: None,
                            },
                            abi::Field {
                                name: "age".to_owned(),
                                kind: ValueKind::Uint(BitWidth::new(16).unwrap()),
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                outputs: vec![],
                state_mutability: StateMutability::NonPayable,
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "getPerson".to_owned(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "id".to_owned(),
                        kind: ValueKind::UINT,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                outputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "".to_owned(),
                        kind: ValueKind::Tuple(vec![
                            ValueKind::String,
                            ValueKind::Uint(BitWidth::new(16).unwrap()),
                        ]),
                        components: Some(vec![
                            abi::Field {
                                name: "name".to_owned(),
                                kind: ValueKind::String,
                                components: None,
                                internal_type: None,
                            },
                            abi::Field {
                                name: "age".to_owned(),
                                kind: ValueKind::Uint(BitWidth::new(16).unwrap()),
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                }],
                state_mutability: StateMutability::View,
            }),
            abi::Descriptor::Event(abi::EventDescriptor {
                name: "PersonAdded".to_owned(),
                inputs: vec![
                    abi::EventField {
                        field: abi::Field {
                            name: "id".to_owned(),
                            kind: ValueKind::UINT,
                            components: None,
                            internal_type: None,
                        },
                        indexed: true,
                    },
                    abi::EventField {
                        field: abi::Field {
                            name: "person".to_owned(),
                            kind: ValueKind::Tuple(vec![
                                ValueKind::String,
                                ValueKind::Uint(BitWidth::new(16).unwrap()),
                            ]),
                            components: Some(vec![
                                abi::Field {
                                    name: "name".to_owned(),
                                    kind: ValueKind::String,
                                    components: None,
                                    internal_type: None,
                                },
                                abi::Field {
                                    name: "age".to_owned(),
                                    kind: ValueKind::Uint(BitWidth::new(16).unwrap()),
                                    components: None,
                                    internal_type: None,
                                },
                            ]),
                            internal_type: None,
                        },
                        indexed: false,
                    },
                ],
                anonymous: false,
            }),
        ];

        assert_eq!(abi, expected);
    }

    #[test]
    fn library_enum_type_handling() {
        let json = json!([
            {
                "inputs": [
                    {
                        "internalType": "enum tuple",
                        "name": "x",
                        "type": "tuple"
                    }
                ],
                "name": "f",
                "outputs": [
                    {
                        "internalType": "enum L.E",
                        "name": "y",
                        "type": "L.E"
                    }
                ],
                "stateMutability": "pure",
                "type": "function"
            },
            {
                "inputs": [
                    {
                        "components": [
                            {
                                "internalType": "uint256",
                                "name": "a",
                                "type": "uint256"
                            },
                            {
                                "internalType": "enum tuple",
                                "name": "b",
                                "type": "tuple"
                            }
                        ],
                        "internalType": "struct L.S[][3]",
                        "name": "x",
                        "type": "tuple[][3]"
                    }
                ],
                "name": "g",
                "outputs": [
                    {
                        "components": [
                            {
                                "internalType": "uint256",
                                "name": "a",
                                "type": "uint256"
                            },
                            {
                                "internalType": "enum L.E",
                                "name": "b",
                                "type": "L.E"
                            }
                        ],
                        "internalType": "struct L.T[3][]",
                        "name": "y",
                        "type": "tuple[3][]"
                    }
                ],
                "stateMutability": "pure",
                "type": "function"
            }
        ]);

        let abi = vec![
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "f".to_string(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "x".to_string(),
                        kind: ValueKind::ENUM,
                        components: None,
                        internal_type: Some("enum tuple".to_string()),
                    },
                    kind_name: Some("tuple".to_string()),
                }],
                outputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "y".to_string(),
                        kind: ValueKind::ENUM,
                        components: None,
                        internal_type: Some("enum L.E".to_string()),
                    },
                    kind_name: Some("L.E".to_string()),
                }],
                state_mutability: StateMutability::Pure,
            }),
            abi::Descriptor::Function(abi::FunctionDescriptor {
                name: "g".to_string(),
                inputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "x".to_string(),
                        kind: ValueKind::FixedArray(
                            3,
                            Box::new(ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                                ValueKind::UINT,
                                ValueKind::ENUM,
                            ])))),
                        ),
                        components: Some(vec![
                            abi::Field {
                                name: "a".to_string(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: Some("uint256".to_string()),
                            },
                            abi::Field {
                                name: "b".to_string(),
                                kind: ValueKind::ENUM,
                                components: None,
                                internal_type: Some("enum tuple".to_string()),
                            },
                        ]),
                        internal_type: Some("struct L.S[][3]".to_string()),
                    },
                    kind_name: Some("(uint256,tuple)[][3]".to_string()),
                }],
                outputs: vec![abi::Parameter {
                    field: abi::Field {
                        name: "y".to_string(),
                        kind: ValueKind::Array(Box::new(ValueKind::FixedArray(
                            3,
                            Box::new(ValueKind::Tuple(vec![ValueKind::UINT, ValueKind::ENUM])),
                        ))),
                        components: Some(vec![
                            abi::Field {
                                name: "a".to_string(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: Some("uint256".to_string()),
                            },
                            abi::Field {
                                name: "b".to_string(),
                                kind: ValueKind::ENUM,
                                components: None,
                                internal_type: Some("enum L.E".to_string()),
                            },
                        ]),
                        internal_type: Some("struct L.T[3][]".to_string()),
                    },
                    kind_name: Some("(uint256,L.E)[3][]".to_string()),
                }],
                state_mutability: StateMutability::Pure,
            }),
        ];

        assert_eq!(abi, serde_json::from_value::<Abi>(json).unwrap());
    }
}
