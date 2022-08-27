//! Solidity-like declaration to ABI descriptor parsing.
//!
//! This module implements a similar ABI parsing language as the "Human-
//! readable ABI" that is available with Ethers.js.
//!
//! Note that the parser implemented in this module is **very** rudimentary.
//! While it is expected to work in the general case, I don't expect it to be
//! very difficult to trick it into doing unexpected things, and it doesn't
//! support all kinds of ABI specs, namely library function descriptors:
//! - `enum` parameters
//! - `storage`-qualified parameters
//!
//! **This code is not robust!** You have been warned...

use super::{
    ConstructorDescriptor, Descriptor, ErrorDescriptor, EventDescriptor, EventField, Field,
    FunctionDescriptor, Parameter, StateMutability,
};
use crate::value::ValueKind;
use std::fmt::{self, Display, Formatter};

/// A Solidity declaration parsing result.
pub type Result<T, E = ParseError> = std::result::Result<T, E>;

/// Parses a declaration to an ABI descriptor.
pub fn parse(s: &str) -> Result<Descriptor> {
    if s.starts_with("function") {
        parse_function(s).map(Descriptor::Function)
    } else if s.starts_with("constructor") {
        parse_constructor(s).map(Descriptor::Constructor)
    } else if s.starts_with("event") {
        parse_event(s).map(Descriptor::Event)
    } else if s.starts_with("error") {
        parse_error(s).map(Descriptor::Error)
    } else {
        Err(ParseError {
            kind: ParseErrorKind::Expect("declaration"),
            pos: 0,
        })
    }
}

/// Parses a declaration to a function descriptor.
pub fn parse_function(s: &str) -> Result<FunctionDescriptor> {
    Parser::exec(s, |p| p.function())
}

/// Parses a declaration to a constructor descriptor.
pub fn parse_constructor(s: &str) -> Result<ConstructorDescriptor> {
    Parser::exec(s, |p| p.constructor())
}

/// Parses a declaration to an event descriptor.
pub fn parse_event(s: &str) -> Result<EventDescriptor> {
    Parser::exec(s, |p| p.event())
}

/// Parses a declaration to an error descriptor.
pub fn parse_error(s: &str) -> Result<ErrorDescriptor> {
    Parser::exec(s, |p| p.error())
}

/// Internal parser type that keeps a string buffer and the parser's current
/// position (which is used for error reporting).
#[derive(Clone)]
struct Parser<'a> {
    buf: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn exec<T, F>(buf: &'a str, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let mut parser = Self::new(buf);
        let result = f(&mut parser)?;
        parser.end()?;
        Ok(result)
    }

    fn new(buf: &'a str) -> Self {
        Self { buf, pos: 0 }
    }

    fn err(&self, kind: ParseErrorKind) -> ParseError {
        ParseError {
            kind,
            pos: self.pos,
        }
    }

    fn eos(&self) -> ParseError {
        self.err(ParseErrorKind::Eos)
    }

    fn expected(&self, name: &'static str) -> ParseError {
        self.err(ParseErrorKind::Expect(name))
    }

    fn rest(&self) -> &'a str {
        unsafe { self.buf.get_unchecked(self.pos..) }
    }

    fn tri<T, F>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&Self, &mut Self) -> Result<T>,
    {
        let mut sub = self.clone();
        let result = f(self, &mut sub)?;
        *self = sub;
        Ok(result)
    }

    fn take(&mut self, c: char) -> Result<()> {
        self.tri(|_, sub| {
            let _ = sub.whitespace();
            let next = sub.rest().chars().next().ok_or_else(|| sub.eos())?;
            if next != c {
                return Err(sub.err(ParseErrorKind::ExpectChar(c)));
            }
            sub.pos += next.len_utf8();
            Ok(())
        })
    }

    fn scan<T, F>(&mut self, state: T, mut f: F) -> &'a str
    where
        F: FnMut(&mut T, char) -> bool,
    {
        let start = self.pos;
        self.pos += self
            .rest()
            .chars()
            .scan(state, |state, c| match f(state, c) {
                true => Some(c),
                false => None,
            })
            .map(char::len_utf8)
            .sum::<usize>();

        unsafe { self.buf.get_unchecked(start..self.pos) }
    }

    fn whitespace(&mut self) -> &'a str {
        self.scan((), |_, c| c.is_whitespace())
    }

    fn word(&mut self) -> &'a str {
        let _ = self.whitespace();
        self.scan(true, |first, c| {
            if *first {
                *first = false;
                matches!(c, 'a'..='z' | 'A'..='Z' | '$' | '_')
            } else {
                matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_')
            }
        })
    }

    fn ident(&mut self) -> Result<&'a str> {
        const KEYWORDS: &[&str] = &[
            // TODO(nlordell): Come up with a reasonable list of keywords. I
            // didn't do this since the value if arguably limited.
        ];

        self.tri(|this, sub| {
            let word = sub.word();
            if word.is_empty() || KEYWORDS.iter().copied().any(|k| k == word) {
                return Err(this.expected("identifier"));
            }
            Ok(word)
        })
    }

    fn keyword(&mut self, keyword: &'static str) -> Result<()> {
        self.keyword_of(keyword, &[keyword])?;
        Ok(())
    }

    fn keyword_of(&mut self, name: &'static str, keywords: &[&str]) -> Result<&'a str> {
        self.tri(|this, sub| {
            let word = sub.word();
            if keywords.iter().copied().all(|k| k != word) {
                return Err(this.expected(name));
            }
            Ok(word)
        })
    }

    fn field_kind(&mut self) -> Result<(ValueKind, Option<Vec<Field>>)> {
        self.tri(|_, sub| {
            let _ = sub.whitespace();
            if let Some('(') = sub.rest().chars().next() {
                let (kind, components) = sub.tuple_kind()?;
                Ok((kind, Some(components)))
            } else {
                let kind = sub.tri(|this, ssub| {
                    ssub.scan((), |_, c| !c.is_whitespace() && !matches!(c, ',' | ')'))
                        .parse()
                        .map_err(|_| this.expected("Solidity type"))
                })?;
                Ok((kind, None))
            }
        })
    }

    fn tuple_kind(&mut self) -> Result<(ValueKind, Vec<Field>)> {
        self.tri(|_, sub| {
            let components = sub.fields(Self::field)?;
            let mut kind = ValueKind::Tuple(
                components
                    .iter()
                    .map(|component| component.kind.clone())
                    .collect(),
            );
            while let Ok(array) = sub.array_suffix() {
                kind = match array {
                    Some(n) => ValueKind::FixedArray(n, Box::new(kind)),
                    None => ValueKind::Array(Box::new(kind)),
                };
            }

            Ok((kind, components))
        })
    }

    fn array_suffix(&mut self) -> Result<Option<usize>> {
        self.tri(|_, sub| {
            sub.take('[')?;
            let n = match sub.scan((), |_, c| c.is_ascii_digit()) {
                "" => None,
                n => Some(n.parse().map_err(|_| sub.expected("fixed array length"))?),
            };
            sub.take(']')?;

            Ok(n)
        })
    }

    fn field(&mut self) -> Result<Field> {
        let (kind, components) = self.field_kind()?;
        let name = self.ident().unwrap_or_default().to_owned();
        Ok(Field {
            name,
            kind,
            components,
            internal_type: None,
        })
    }

    fn parameter_field(&mut self) -> Result<Parameter> {
        // TODO(nlordell): Support Library functions with named enums and
        // storage parameter location.

        let (kind, components) = self.field_kind()?;
        if can_specify_location(&kind) {
            let _ = self.keyword_of("parameter location", &["calldata", "memory"]);
        }
        let name = self.ident().unwrap_or_default().to_owned();
        Ok(Parameter {
            field: Field {
                name,
                kind,
                components,
                internal_type: None,
            },
            kind_name: None,
        })
    }

    fn event_field(&mut self) -> Result<EventField> {
        let (kind, components) = self.field_kind()?;
        let indexed = self.keyword("indexed").is_ok();
        let name = self.ident().unwrap_or_default().to_owned();
        Ok(EventField {
            field: Field {
                name,
                kind,
                components,
                internal_type: None,
            },
            indexed,
        })
    }

    fn fields<T, F>(&mut self, mut f: F) -> Result<Vec<T>>
    where
        F: FnMut(&mut Self) -> Result<T>,
    {
        self.tri(|_, sub| {
            sub.take('(')?;
            let mut fields = Vec::new();
            while let Ok(field) = if fields.is_empty() {
                f(sub)
            } else {
                sub.tri(|_, ssub| ssub.take(',').and_then(|_| f(ssub)))
            } {
                fields.push(field);
            }
            sub.take(')')?;

            Ok(fields)
        })
    }

    fn function(&mut self) -> Result<FunctionDescriptor> {
        self.keyword("function")?;
        let name = self.ident()?.to_owned();
        let inputs = self.fields(Self::parameter_field)?;
        let state_mutability =
            match self.keyword_of("state mutability", &["pure", "view", "payable"]) {
                Ok("pure") => StateMutability::Pure,
                Ok("view") => StateMutability::View,
                Ok("payable") => StateMutability::Payable,
                _ => StateMutability::NonPayable,
            };
        let outputs = match self.keyword("returns") {
            Ok(_) => self.fields(Self::parameter_field)?,
            Err(_) => vec![],
        };
        Ok(FunctionDescriptor {
            name,
            inputs,
            outputs,
            state_mutability,
        })
    }

    fn constructor(&mut self) -> Result<ConstructorDescriptor> {
        self.keyword("constructor")?;
        let inputs = self.fields(Self::parameter_field)?;
        let state_mutability = match self.keyword("payable") {
            Ok(_) => StateMutability::Payable,
            Err(_) => StateMutability::NonPayable,
        };

        Ok(ConstructorDescriptor {
            inputs,
            state_mutability,
        })
    }

    fn event(&mut self) -> Result<EventDescriptor> {
        self.keyword("event")?;
        let name = self.ident()?.to_owned();
        let inputs = self.fields(Self::event_field)?;
        let anonymous = self.keyword("anonymous").is_ok();

        Ok(EventDescriptor {
            name,
            inputs,
            anonymous,
        })
    }

    fn error(&mut self) -> Result<ErrorDescriptor> {
        self.keyword("error")?;
        let name = self.ident()?.to_owned();
        let inputs = self.fields(Self::field)?;

        Ok(ErrorDescriptor { name, inputs })
    }

    fn end(mut self) -> Result<()> {
        let _ = self.take(';');
        let _ = self.whitespace();
        if !self.rest().is_empty() {
            return Err(self.expected("end of declaration"));
        }

        Ok(())
    }
}

/// A declaration parsing error.
#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    pos: usize,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} at position {}", self.kind, self.pos)
    }
}

/// Internal message representation of parsing error.
///
/// This allows us to make these errors freely without any allocations or
/// performance impacts.
#[derive(Debug)]
enum ParseErrorKind {
    Eos,
    Expect(&'static str),
    ExpectChar(char),
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseErrorKind::Eos => f.write_str("unexpected end of stream"),
            ParseErrorKind::Expect(name) => write!(f, "expected {name}"),
            ParseErrorKind::ExpectChar(char) => write!(f, "expected {char}"),
        }
    }
}

fn can_specify_location(kind: &ValueKind) -> bool {
    matches!(
        kind,
        ValueKind::FixedArray(..)
            | ValueKind::Bytes
            | ValueKind::String
            | ValueKind::Array(_)
            | ValueKind::Tuple(_)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_function() {
        let parsed = parse_function(
            r#"
            function f(
                address from,
                (address, uint256 value)[] memory balances
            ) returns (
                bool,
                (address, uint256 x) y
            );
            "#,
        )
        .unwrap();

        let expected = FunctionDescriptor {
            name: "f".to_owned(),
            inputs: vec![
                Parameter {
                    field: Field {
                        name: "from".to_string(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                },
                Parameter {
                    field: Field {
                        name: "balances".to_string(),
                        kind: ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                            ValueKind::Address,
                            ValueKind::UINT,
                        ]))),
                        components: Some(vec![
                            Field {
                                name: "".to_owned(),
                                kind: ValueKind::Address,
                                components: None,
                                internal_type: None,
                            },
                            Field {
                                name: "value".to_owned(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                },
            ],
            outputs: vec![
                Parameter {
                    field: Field {
                        name: "".to_string(),
                        kind: ValueKind::Bool,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                },
                Parameter {
                    field: Field {
                        name: "y".to_string(),
                        kind: ValueKind::Tuple(vec![ValueKind::Address, ValueKind::UINT]),
                        components: Some(vec![
                            Field {
                                name: "".to_owned(),
                                kind: ValueKind::Address,
                                components: None,
                                internal_type: None,
                            },
                            Field {
                                name: "x".to_owned(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                },
            ],
            state_mutability: StateMutability::NonPayable,
        };

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parses_function_without_returns() {
        let expected = FunctionDescriptor {
            name: "f".to_owned(),
            inputs: vec![],
            outputs: vec![],
            state_mutability: StateMutability::NonPayable,
        };

        for function in ["function f()", "function f() returns ()"] {
            let parsed = parse_function(function).unwrap();
            assert_eq!(parsed, expected);
        }
    }

    #[test]
    fn parses_function_state_mutability() {
        for (function, state_mutability) in [
            ("function f() pure", StateMutability::Pure),
            ("function f() view returns (bool)", StateMutability::View),
            ("function f() payable", StateMutability::Payable),
            ("function f()", StateMutability::NonPayable),
            ("function f() returns (bool)", StateMutability::NonPayable),
        ] {
            let parsed = parse_function(function).unwrap();
            assert_eq!(parsed.state_mutability, state_mutability);
        }
    }

    #[test]
    fn parses_constructor() {
        let parsed = parse_constructor(
            r#"
            constructor (address from, (address, uint256 value)[] memory balances);
            "#,
        )
        .unwrap();

        let expected = ConstructorDescriptor {
            inputs: vec![
                Parameter {
                    field: Field {
                        name: "from".to_string(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    kind_name: None,
                },
                Parameter {
                    field: Field {
                        name: "balances".to_string(),
                        kind: ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                            ValueKind::Address,
                            ValueKind::UINT,
                        ]))),
                        components: Some(vec![
                            Field {
                                name: "".to_owned(),
                                kind: ValueKind::Address,
                                components: None,
                                internal_type: None,
                            },
                            Field {
                                name: "value".to_owned(),
                                kind: ValueKind::UINT,
                                components: None,
                                internal_type: None,
                            },
                        ]),
                        internal_type: None,
                    },
                    kind_name: None,
                },
            ],
            state_mutability: StateMutability::NonPayable,
        };

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parses_payable_constructor() {
        let parsed = parse_constructor(
            r#"
            constructor(bytes calldata) payable
            "#,
        )
        .unwrap();

        let expected = ConstructorDescriptor {
            inputs: vec![Parameter {
                field: Field {
                    name: "".to_string(),
                    kind: ValueKind::Bytes,
                    components: None,
                    internal_type: None,
                },
                kind_name: None,
            }],
            state_mutability: StateMutability::Payable,
        };

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parses_event() {
        let parsed = parse_event(
            r#"
            event Transfer(address indexed from, address indexed to, uint256 value);
            "#,
        )
        .unwrap();

        let expected = EventDescriptor {
            name: "Transfer".to_owned(),
            inputs: vec![
                EventField {
                    field: Field {
                        name: "from".to_string(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    indexed: true,
                },
                EventField {
                    field: Field {
                        name: "to".to_string(),
                        kind: ValueKind::Address,
                        components: None,
                        internal_type: None,
                    },
                    indexed: true,
                },
                EventField {
                    field: Field {
                        name: "value".to_owned(),
                        kind: ValueKind::UINT,
                        components: None,
                        internal_type: None,
                    },
                    indexed: false,
                },
            ],
            anonymous: false,
        };

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parses_anonymous_event() {
        assert_eq!(
            parse_event("event Foo() anonymous").unwrap(),
            EventDescriptor {
                name: "Foo".to_owned(),
                inputs: vec![],
                anonymous: true,
            }
        );
    }

    #[test]
    fn parses_error_declaration() {
        let parsed = parse_error(
            r#"
            error MyError(
                uint256 someField,
                (
                    address sub,
                    (
                        uint256 subSub,
                        bool
                    )[] subStruct
                )[][4] someStruct
            );
            "#,
        )
        .unwrap();

        let expected = ErrorDescriptor {
            name: "MyError".to_owned(),
            inputs: vec![
                Field {
                    name: "someField".to_owned(),
                    kind: ValueKind::UINT,
                    components: None,
                    internal_type: None,
                },
                Field {
                    name: "someStruct".to_owned(),
                    kind: ValueKind::FixedArray(
                        4,
                        Box::new(ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                            ValueKind::Address,
                            ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                                ValueKind::UINT,
                                ValueKind::Bool,
                            ]))),
                        ])))),
                    ),
                    components: Some(vec![
                        Field {
                            name: "sub".to_owned(),
                            kind: ValueKind::Address,
                            components: None,
                            internal_type: None,
                        },
                        Field {
                            name: "subStruct".to_owned(),
                            kind: ValueKind::Array(Box::new(ValueKind::Tuple(vec![
                                ValueKind::UINT,
                                ValueKind::Bool,
                            ]))),
                            components: Some(vec![
                                Field {
                                    name: "subSub".to_owned(),
                                    kind: ValueKind::UINT,
                                    components: None,
                                    internal_type: None,
                                },
                                Field {
                                    name: "".to_owned(),
                                    kind: ValueKind::Bool,
                                    components: None,
                                    internal_type: None,
                                },
                            ]),
                            internal_type: None,
                        },
                    ]),
                    internal_type: None,
                },
            ],
        };

        assert_eq!(parsed, expected);
    }
}
