//! Solidity ABI descriptors.

pub mod declaration;
mod json;

use crate::{function::Selector, primitive::Word, value::ValueKind};
use ethprim::Keccak;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Display, Formatter, Write as _};

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

impl FunctionDescriptor {
    /// Returns the error decriptor's canonical formatter.
    pub fn canonical(&self) -> Canonical<Self> {
        Canonical(self)
    }

    /// Returns a display formatter for the full function signature.
    ///
    /// This is different than its canonical representation in that it also
    /// includes the return types. Function selectors are computed by hashing
    /// the canonical representation without the return types.
    pub fn signature(&self) -> Signature {
        Signature(self)
    }

    /// Computes the selector the error type.
    pub fn selector(&self) -> Selector {
        let mut hasher = Keccak::new();
        write!(&mut hasher, "{}", self.canonical()).unwrap();
        selector(hasher)
    }
}

impl Display for Canonical<'_, FunctionDescriptor> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&self.0.name)?;
        fmt_fields(f, &self.0.inputs)
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

impl EventDescriptor {
    /// Returns the error decriptor's canonical formatter.
    pub fn canonical(&self) -> Canonical<Self> {
        Canonical(self)
    }

    /// Computes the selector the error type.
    pub fn selector(&self) -> Option<Word> {
        if self.anonymous {
            return None;
        }

        let mut hasher = Keccak::new();
        write!(&mut hasher, "{}", self.canonical()).unwrap();
        Some(topic(hasher))
    }
}

impl Display for Canonical<'_, EventDescriptor> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&self.0.name)?;
        fmt_fields(f, &self.0.inputs)
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

impl ErrorDescriptor {
    /// Returns the error decriptor's canonical formatter.
    pub fn canonical(&self) -> Canonical<Self> {
        Canonical(self)
    }

    /// Computes the selector the error type.
    pub fn selector(&self) -> Selector {
        let mut hasher = Keccak::new();
        write!(&mut hasher, "{}", self.canonical()).unwrap();
        selector(hasher)
    }
}

impl Display for Canonical<'_, ErrorDescriptor> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(&self.0.name)?;
        fmt_fields(f, &self.0.inputs)
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

impl Display for Canonical<'_, Field> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0.kind)
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

impl Display for Canonical<'_, Parameter> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &self.0.kind_name {
            Some(kind_name) => f.write_str(kind_name),
            None => write!(f, "{}", Canonical(&self.0.field)),
        }
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

impl Display for Canonical<'_, EventField> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", Canonical(&self.0.field))
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
    fn with_flags(constant: bool, payable: bool) -> StateMutability {
        match (constant, payable) {
            (_, true) => Self::Payable,
            (false, _) => Self::NonPayable,
            (true, _) => Self::View,
        }
    }
}

/// Formatter for the canonical representation of an ABI descriptor.
///
/// This is the string representation that is used for hashing in order to
/// compute selectors.
pub struct Canonical<'a, T>(&'a T);

/// A function signature formatter.
pub struct Signature<'a>(&'a FunctionDescriptor);

impl Display for Signature<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:", Canonical(self.0))?;
        fmt_fields(f, &self.0.outputs)
    }
}

fn fmt_fields<'a, T, I>(f: &mut Formatter, fields: I) -> fmt::Result
where
    T: 'a,
    Canonical<'a, T>: Display,
    I: IntoIterator<Item = &'a T> + 'a,
{
    f.write_str("(")?;
    for (i, field) in fields.into_iter().enumerate() {
        if i != 0 {
            f.write_str(",")?;
        }
        write!(f, "{}", Canonical(field))?;
    }
    f.write_str(")")
}

fn selector(hasher: Keccak) -> Selector {
    let digest = hasher.finalize();
    Selector(digest[..4].try_into().unwrap())
}

fn topic(hasher: Keccak) -> [u8; 32] {
    *hasher.finalize()
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn function_selector() {
        let function =
            FunctionDescriptor::parse_declaration("function transfer(address to, uint value)")
                .unwrap();
        assert_eq!(function.selector(), hex!("a9059cbb"));
    }

    #[test]
    fn function_signature() {
        let function = FunctionDescriptor::parse_declaration(
            "function transfer(address to, uint value) returns (bool)",
        )
        .unwrap();
        assert_eq!(
            function.signature().to_string(),
            "transfer(address,uint256):(bool)",
        );
    }

    #[test]
    fn event_selector() {
        let event = EventDescriptor::parse_declaration(
            "event Transfer(address indexed to, address indexed from, uint256 value)",
        )
        .unwrap();
        assert_eq!(
            event.selector().unwrap(),
            hex!("ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"),
        );
    }

    #[test]
    fn anonymous_event_selector() {
        let event = EventDescriptor::parse_declaration("event Foo() anonymous").unwrap();
        assert!(event.selector().is_none());
    }

    #[test]
    fn error_selector() {
        let error = ErrorDescriptor::parse_declaration("error Error(string message)").unwrap();
        assert_eq!(error.selector(), hex!("08c379a0"));
    }
}
