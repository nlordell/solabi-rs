//! Solidity ABI descriptors.

pub mod declaration;
mod json;

pub use self::json::StateMutability;
use crate::value::ValueKind;
use serde::{Deserialize, Serialize};

/// A Solidity ABI - i.e. a vector of descriptors.
pub type Abi = Vec<Descriptor>;

/// A Solidity ABI descriptor.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Descriptor", into = "json::Descriptor")]
pub enum Descriptor {
    /// A function.
    Function(FunctionDescriptor),
    /// A constructor.
    Constructor(ConstructorDescriptor),
    /// A "receive Ether" function.
    Receive,
    /// A fallback function.
    Fallback(StateMutability),
    /// An event.
    Event(EventDescriptor),
    /// An error.
    Error(ErrorDescriptor),
}

impl TryFrom<json::Descriptor> for Descriptor {
    type Error = &'static str;

    fn try_from(
        value: json::Descriptor,
    ) -> Result<Self, <Descriptor as TryFrom<json::Descriptor>>::Error> {
        match &value.kind {
            json::DescriptorKind::Function => value.try_into().map(Self::Function),
            json::DescriptorKind::Constructor => value.try_into().map(Self::Constructor),
            json::DescriptorKind::Receive => Ok(Self::Receive),
            json::DescriptorKind::Fallback => Ok(Self::Fallback(value.state_mutability())),
            json::DescriptorKind::Event => value.try_into().map(Self::Event),
            json::DescriptorKind::Error => value.try_into().map(Self::Error),
        }
    }
}

impl From<Descriptor> for json::Descriptor {
    fn from(descriptor: Descriptor) -> Self {
        match descriptor {
            Descriptor::Function(function) => function.into(),
            Descriptor::Constructor(constructor) => constructor.into(),
            Descriptor::Receive => Self {
                state_mutability: Some(StateMutability::Payable),
                ..json::DescriptorKind::Receive.into()
            },
            Descriptor::Fallback(state_mutability) => Self {
                state_mutability: Some(state_mutability),
                ..json::DescriptorKind::Fallback.into()
            },
            Descriptor::Event(event) => event.into(),
            Descriptor::Error(error) => error.into(),
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

/// A function descriptor.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Descriptor", into = "json::Descriptor")]
pub struct FunctionDescriptor {
    /// The name of the function.
    pub name: String,
    /// The function input parameters.
    pub inputs: Vec<Parameter>,
    /// The function ouput parameters (i.e. `returns` values).
    pub outputs: Vec<Parameter>,
    /// The function state mutability.
    pub state_mutability: StateMutability,
}

impl TryFrom<json::Descriptor> for FunctionDescriptor {
    type Error = &'static str;

    fn try_from(value: json::Descriptor) -> Result<Self, Self::Error> {
        check!(
            value.kind == json::DescriptorKind::Function,
            "not a function"
        );
        let state_mutability = value.state_mutability();
        Ok(Self {
            name: value.name.ok_or("function missing name")?,
            inputs: from_fields(value.inputs.ok_or("function missing inputs")?)?,
            outputs: from_fields(value.outputs.ok_or("function missing outputs")?)?,
            state_mutability,
        })
    }
}

impl From<FunctionDescriptor> for json::Descriptor {
    fn from(function: FunctionDescriptor) -> Self {
        Self {
            name: Some(function.name),
            inputs: Some(to_fields(function.inputs)),
            outputs: Some(to_fields(function.outputs)),
            state_mutability: Some(function.state_mutability),
            ..json::DescriptorKind::Function.into()
        }
    }
}

/// A constructor descriptor.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Descriptor", into = "json::Descriptor")]
pub struct ConstructorDescriptor {
    /// The function input parameters.
    pub inputs: Vec<Parameter>,
    /// The function state mutability.
    pub state_mutability: StateMutability,
}

impl TryFrom<json::Descriptor> for ConstructorDescriptor {
    type Error = &'static str;

    fn try_from(value: json::Descriptor) -> Result<Self, Self::Error> {
        check!(
            value.kind == json::DescriptorKind::Constructor,
            "not a constructor"
        );
        let state_mutability = value.state_mutability();
        Ok(Self {
            inputs: from_fields(value.inputs.ok_or("constructor missing inputs")?)?,
            state_mutability,
        })
    }
}

impl From<ConstructorDescriptor> for json::Descriptor {
    fn from(constructor: ConstructorDescriptor) -> Self {
        Self {
            inputs: Some(to_fields(constructor.inputs)),
            state_mutability: Some(constructor.state_mutability),
            ..json::DescriptorKind::Constructor.into()
        }
    }
}

/// An event descriptor.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Descriptor", into = "json::Descriptor")]
pub struct EventDescriptor {
    /// The name of the event.
    pub name: String,
    /// The event fields.
    pub inputs: Vec<EventField>,
    /// Whether or not the event is anonymous.
    pub anonymous: bool,
}

impl TryFrom<json::Descriptor> for EventDescriptor {
    type Error = &'static str;

    fn try_from(value: json::Descriptor) -> Result<Self, Self::Error> {
        check!(value.kind == json::DescriptorKind::Event, "not an event");
        Ok(Self {
            name: value.name.ok_or("event missing name")?,
            inputs: from_fields(value.inputs.ok_or("event missing inputs")?)?,
            anonymous: value.anonymous.unwrap_or_default(),
        })
    }
}

impl From<EventDescriptor> for json::Descriptor {
    fn from(event: EventDescriptor) -> Self {
        Self {
            name: Some(event.name),
            inputs: Some(to_fields(event.inputs)),
            anonymous: Some(event.anonymous),
            ..json::DescriptorKind::Event.into()
        }
    }
}

/// An error descriptor.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Descriptor", into = "json::Descriptor")]
pub struct ErrorDescriptor {
    /// The name of the event.
    pub name: String,
    /// The event fields.
    pub inputs: Vec<Field>,
}

impl TryFrom<json::Descriptor> for ErrorDescriptor {
    type Error = &'static str;

    fn try_from(value: json::Descriptor) -> Result<Self, Self::Error> {
        check!(value.kind == json::DescriptorKind::Error, "not an error");
        Ok(Self {
            name: value.name.ok_or("error missing name")?,
            inputs: from_fields(value.inputs.ok_or("error missing inputs")?)?,
        })
    }
}

impl From<ErrorDescriptor> for json::Descriptor {
    fn from(error: ErrorDescriptor) -> Self {
        Self {
            name: Some(error.name),
            inputs: Some(to_fields(error.inputs)),
            ..json::DescriptorKind::Error.into()
        }
    }
}

/// A field.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Field", into = "json::Field")]
pub struct Field {
    /// The name of the field.
    pub name: String,
    /// The canonical type of the field.
    pub kind: ValueKind,
    /// The components of a tuple.
    ///
    /// This is for information purposes of the individual fields.
    pub components: Option<Vec<Field>>,
    /// The internal type name.
    ///
    /// This is for informational purposes (can be useful, for example, in the
    /// context of code generation of user defined types and structures).
    pub internal_type: Option<String>,
}

impl TryFrom<json::Field> for Field {
    type Error = &'static str;

    fn try_from(value: json::Field) -> Result<Self, Self::Error> {
        let kind = (&value).try_into()?;
        Ok(Self {
            name: value.name,
            kind,
            components: value
                .components
                .map(|components| {
                    components
                        .into_iter()
                        .map(Field::try_from)
                        .collect::<Result<_, _>>()
                })
                .transpose()?,
            internal_type: value.internal_type,
        })
    }
}

impl TryFrom<&'_ json::Field> for ValueKind {
    type Error = &'static str;

    /// Converts a tuple kind to its value kind.
    fn try_from(value: &'_ json::Field) -> Result<Self, Self::Error> {
        match &value.kind {
            json::FieldKind::Value(kind) => Ok(kind.clone()),
            json::FieldKind::Tuple(kind) => {
                let components = value
                    .components
                    .as_ref()
                    .ok_or("missing components for tuple field")?
                    .iter()
                    .map(ValueKind::try_from)
                    .collect::<Result<_, _>>()?;
                Ok(kind.to_value(components))
            }
            json::FieldKind::Other(_) => Ok(ValueKind::ENUM),
        }
    }
}

impl From<Field> for json::Field {
    fn from(field: Field) -> Self {
        let (kind, components) = match json::TupleKind::from_value(&field.kind) {
            Some((kind, fields)) => {
                let components = match field.components {
                    Some(components) => components.into_iter().map(json::Field::from).collect(),
                    None => fields.iter().map(json::Field::from).collect(),
                };
                (json::FieldKind::Tuple(kind), Some(components))
            }
            None => (json::FieldKind::Value(field.kind), None),
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

impl From<&'_ ValueKind> for json::Field {
    fn from(kind: &ValueKind) -> Self {
        let (kind, components) = match json::TupleKind::from_value(kind) {
            Some((kind, fields)) => (
                json::FieldKind::Tuple(kind),
                Some(fields.iter().map(json::Field::from).collect()),
            ),
            None => (json::FieldKind::Value(kind.clone()), None),
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

/// A input or output parameter.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Field", into = "json::Field")]
pub struct Parameter {
    /// The field.
    pub field: Field,
    /// The type name used for computing function selectors.
    ///
    /// Typically, this can be determined by the parameter type (i.e. the `kind`
    /// field). However, specifically for `library` ABIs, this is not the case.
    /// Specifically:
    /// - `enum` parameters use the type name instead of `uint8`
    /// - `storage` parameters have " storage" appended to the canonical name.
    pub kind_name: Option<String>,
}

impl TryFrom<json::Field> for Parameter {
    type Error = &'static str;

    fn try_from(value: json::Field) -> Result<Self, Self::Error> {
        let value = value.fixup();
        let kind_name = value.library_parameter_type();
        Ok(Self {
            field: value.try_into()?,
            kind_name,
        })
    }
}

impl From<Parameter> for json::Field {
    fn from(parameter: Parameter) -> Self {
        let mut field = json::Field::from(parameter.field);
        if let Some(kind_name) = parameter.kind_name {
            field.kind = json::FieldKind::Other(kind_name);
        }
        field
    }
}

/// An event field.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "json::Field", into = "json::Field")]
pub struct EventField {
    /// The field.
    pub field: Field,
    /// Whether or not the field is part of the EVM log's topics.
    pub indexed: bool,
}

impl TryFrom<json::Field> for EventField {
    type Error = &'static str;

    fn try_from(value: json::Field) -> Result<Self, Self::Error> {
        let indexed = value.indexed.unwrap_or_default();
        Ok(Self {
            field: value.try_into()?,
            indexed,
        })
    }
}

impl From<EventField> for json::Field {
    fn from(field: EventField) -> Self {
        Self {
            indexed: Some(field.indexed),
            ..field.field.into()
        }
    }
}

fn from_fields<T>(fields: Vec<json::Field>) -> Result<Vec<T>, T::Error>
where
    T: TryFrom<json::Field>,
{
    fields.into_iter().map(T::try_from).collect()
}

fn to_fields<T>(fields: Vec<T>) -> Vec<json::Field>
where
    T: Into<json::Field>,
{
    fields.into_iter().map(T::into).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

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
            Descriptor::Function(FunctionDescriptor {
                name: "f".to_string(),
                inputs: vec![Parameter {
                    field: Field {
                        name: "x".to_string(),
                        kind: ValueKind::ENUM,
                        components: None,
                        internal_type: Some("enum tuple".to_string()),
                    },
                    kind_name: Some("tuple".to_string()),
                }],
                outputs: vec![Parameter {
                    field: Field {
                        name: "y".to_string(),
                        kind: ValueKind::ENUM,
                        components: None,
                        internal_type: Some("enum L.E".to_string()),
                    },
                    kind_name: Some("L.E".to_string()),
                }],
                state_mutability: StateMutability::Pure,
            }),
            Descriptor::Function(FunctionDescriptor {
                name: "g".to_string(),
                inputs: vec![Parameter {
                    field: Field {
                        name: "x".to_string(),
                        kind: ValueKind::FixedArray(
                            3,
                            Box::new(ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                                ValueKind::UINT,
                                ValueKind::ENUM,
                            ])))),
                        ),
                        components: Some(vec![
                            Field {
                                name: "a".to_string(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: Some("uint256".to_string()),
                            },
                            Field {
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
                outputs: vec![Parameter {
                    field: Field {
                        name: "y".to_string(),
                        kind: ValueKind::Array(Box::new(ValueKind::FixedArray(
                            3,
                            Box::new(ValueKind::Tuple(vec![ValueKind::UINT, ValueKind::ENUM])),
                        ))),
                        components: Some(vec![
                            Field {
                                name: "a".to_string(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: Some("uint256".to_string()),
                            },
                            Field {
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
