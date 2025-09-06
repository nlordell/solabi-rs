//! Module containing Solidity event related traits and logic.

use crate::{
    decode::{Decode, DecodeError},
    encode::Encode,
    fmt::Hex,
    log::{FromTopic, Log, ToTopic, Topics},
    primitive::Word,
};
use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    marker::PhantomData,
};

/// A trait for converting to and from event indices.
pub trait Indexed: Sized {
    fn from_topics(topics: &Topics) -> Result<(Word, Self), FromTopicsError>;
    fn to_topics(topic0: &Word, indices: &Self) -> Topics;
}

/// A trait for converting to and from event indices.
pub trait IndexedAnonymous: Sized {
    fn from_topics_anonymous(topics: &Topics) -> Result<Self, FromTopicsError>;
    fn to_topics_anonymous(&self) -> Topics;
}

/// An event encoder with a known topic0.
pub struct EventEncoder<I, D> {
    /// The event topic 0.
    pub topic0: Word,
    _marker: PhantomData<*const (I, D)>,
}

impl<I, D> EventEncoder<I, D>
where
    I: Indexed,
{
    /// Creates a new typed event from a topic0.
    pub const fn new(topic0: Word) -> Self {
        Self {
            topic0,
            _marker: PhantomData,
        }
    }
}

impl<I, D> EventEncoder<I, D>
where
    I: Indexed,
    D: Encode,
{
    /// Encode event data into an EVM log.
    pub fn encode(&self, indices: &I, data: &D) -> Log<'_> {
        Log {
            topics: I::to_topics(&self.topic0, indices),
            data: crate::encode(data).into(),
        }
    }
}

impl<I, D> EventEncoder<I, D>
where
    I: Indexed,
    D: Decode,
{
    /// Decode event data from an EVM log.
    pub fn decode(&self, log: &Log) -> Result<(I, D), ParseError> {
        let (topic0, indices) = I::from_topics(&log.topics)?;
        if topic0 != self.topic0 {
            return Err(ParseError::SelectorMismatch(topic0));
        }
        let data = crate::decode(&log.data)?;

        Ok((indices, data))
    }
}

impl<I, D> Debug for EventEncoder<I, D> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_struct("EventEncoder")
            .field("topic0", &Hex(&self.topic0))
            .finish()
    }
}

/// An anonymous event encoder.
pub struct AnonymousEventEncoder<I, D>(PhantomData<*const (I, D)>);

impl<I, D> AnonymousEventEncoder<I, D>
where
    I: IndexedAnonymous,
{
    /// Creates a new anonymous event.
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<I, D> AnonymousEventEncoder<I, D>
where
    I: IndexedAnonymous,
    D: Encode,
{
    /// Encode event data into an EVM log.
    pub fn encode(&self, indices: &I, data: &D) -> Log<'_> {
        Log {
            topics: I::to_topics_anonymous(indices),
            data: crate::encode(data).into(),
        }
    }
}

impl<I, D> AnonymousEventEncoder<I, D>
where
    I: IndexedAnonymous,
    D: Decode,
{
    /// Decode event data from an EVM log.
    pub fn decode(&self, log: &Log) -> Result<(I, D), ParseError> {
        let indices = I::from_topics_anonymous(&log.topics)?;
        let data = crate::decode(&log.data)?;

        Ok((indices, data))
    }
}

impl<I, D> Debug for AnonymousEventEncoder<I, D>
where
    I: IndexedAnonymous,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.debug_tuple("AnonymousEventEncoder").finish()
    }
}

impl<I, D> Default for AnonymousEventEncoder<I, D>
where
    I: IndexedAnonymous,
{
    fn default() -> Self {
        Self::new()
    }
}

/// An error parsing an event.
pub enum ParseError {
    /// An error parsing event indices.
    Topics(FromTopicsError),
    /// The event's topic0 does not match the log's topic0.
    SelectorMismatch(Word),
    /// An error decoding log data.
    Data(DecodeError),
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Topics(err) => f.debug_tuple("FromTopics").field(err).finish(),
            Self::SelectorMismatch(topic0) => f
                .debug_tuple("SelectorMismatch")
                .field(&Hex(topic0))
                .finish(),
            Self::Data(err) => f.debug_tuple("Data").field(err).finish(),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Topics(err) => write!(f, "{err}"),
            Self::SelectorMismatch(_) => f.write_str("event topic0 does not match log's topic0"),
            Self::Data(err) => write!(f, "{err}"),
        }
    }
}

impl Error for ParseError {}

impl From<FromTopicsError> for ParseError {
    fn from(err: FromTopicsError) -> Self {
        Self::Topics(err)
    }
}

impl From<DecodeError> for ParseError {
    fn from(err: DecodeError) -> Self {
        Self::Data(err)
    }
}

/// An error parsing log topics.
#[derive(Debug)]
pub enum FromTopicsError {
    /// The log contains the wrong number of topics.
    WrongCount,
    /// The topic data was invalid.
    InvalidData,
}

impl Display for FromTopicsError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::WrongCount => f.write_str("event indices does not match log topics"),
            Self::InvalidData => f.write_str("log topic data is invalid for event index"),
        }
    }
}

impl Error for FromTopicsError {}

macro_rules! impl_indexed {
    ($($t:ident),*) => {
        #[allow(non_snake_case, unused_variables)]
        impl<$($t),*> Indexed for ($($t,)*)
        where
            $($t: ToTopic + FromTopic,)*
        {
            fn from_topics(topics: &Topics) -> Result<(Word, Self), FromTopicsError> {
                let mut topics = topics.iter().copied();
                let topic0 = topics.next().ok_or(FromTopicsError::WrongCount)?;
                $(let $t = $t::from_topic(topics.next().ok_or(FromTopicsError::WrongCount)?)
                    .ok_or(FromTopicsError::InvalidData)?;)*
                if topics.next().is_some() {
                    return Err(FromTopicsError::WrongCount);
                }
                Ok((topic0, ($($t,)*)))
            }

            fn to_topics(topic0: &Word, ($($t,)*): &Self) -> Topics {
                Topics::from([*topic0, $($t.to_topic()),*])
            }
        }

        impl_indexed! { anonymous: $($t),* }
    };

    (anonymous: $($t:ident),*) => {
        #[allow(non_snake_case, unused_mut, unused_variables)]
        impl<$($t),*> IndexedAnonymous for ($($t,)*)
        where
            $($t: ToTopic + FromTopic,)*
        {
            fn from_topics_anonymous(topics: &Topics) -> Result<Self, FromTopicsError> {
                let mut topics = topics.iter().copied();
                $(let $t = $t::from_topic(topics.next().ok_or(FromTopicsError::WrongCount)?)
                    .ok_or(FromTopicsError::InvalidData)?;)*
                if topics.next().is_some() {
                    return Err(FromTopicsError::WrongCount);
                }
                Ok(($($t,)*))
            }

            fn to_topics_anonymous(&self) -> Topics {
                let ($($t,)*) = self;
                Topics::from([$($t.to_topic()),*])
            }
        }
    };
}

impl_indexed! {}
impl_indexed! { A }
impl_indexed! { A, B }
impl_indexed! { A, B, C }
impl_indexed! { anonymous: A, B, C, D }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytes::Bytes;
    use ethprim::{address, uint, Address, U256};
    use hex_literal::hex;
    use std::borrow::Cow;

    #[test]
    fn transfer_event_roundtrip() {
        let transfer = EventEncoder::<(Address, Address), (U256,)>::new(hex!(
            "ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
        ));

        let from = address!("0x0101010101010101010101010101010101010101");
        let to = address!("0x0202020202020202020202020202020202020202");
        let value = uint!("4_200_000_000_000_000_000");

        let log = Log {
            topics: Topics::from([
                transfer.topic0,
                hex!("0000000000000000000000000101010101010101010101010101010101010101"),
                hex!("0000000000000000000000000202020202020202020202020202020202020202"),
            ]),
            data: hex!("0000000000000000000000000000000000000000000000003a4965bf58a40000")[..]
                .into(),
        };

        assert_eq!(transfer.decode(&log).unwrap(), ((from, to), (value,)),);
        assert_eq!(transfer.encode(&(from, to), &(value,)), log);
    }

    #[test]
    fn fails_to_decode_event_with_different_indices() {
        let selector = hex!("ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef");

        let transfer = EventEncoder::<(Address, Address), (U256,)>::new(selector);
        let log = transfer.encode(
            &(
                address!("0x0101010101010101010101010101010101010101"),
                address!("0x0202020202020202020202020202020202020202"),
            ),
            &(uint!("4_200_000_000_000_000_000"),),
        );

        let transfer = EventEncoder::<(Address, Address, U256), ()>::new(selector);
        assert!(matches!(
            transfer.decode(&log),
            Err(ParseError::Topics(FromTopicsError::WrongCount))
        ));

        let transfer = EventEncoder::<(Address,), (Address, U256)>::new(selector);
        assert!(matches!(
            transfer.decode(&log),
            Err(ParseError::Topics(FromTopicsError::WrongCount))
        ));
    }

    #[test]
    fn empty_indices_and_data() {
        let event = EventEncoder::<(), (U256,)>::new([1; 32]);
        let value = uint!("42");
        let log = Log {
            topics: Topics::from([event.topic0]),
            data: hex!("000000000000000000000000000000000000000000000000000000000000002a")[..]
                .into(),
        };
        assert_eq!(event.decode(&log).unwrap(), ((), (value,)),);
        assert_eq!(event.encode(&(), &(value,)), log);

        let event = EventEncoder::<(U256,), ()>::new([2; 32]);
        let value = uint!("42");
        let log = Log {
            topics: Topics::from([
                event.topic0,
                hex!("000000000000000000000000000000000000000000000000000000000000002a"),
            ]),
            data: hex!("")[..].into(),
        };
        assert_eq!(event.decode(&log).unwrap(), ((value,), ()),);
        assert_eq!(event.encode(&(value,), &()), log);

        let event = EventEncoder::<(), ()>::new([2; 32]);
        let log = Log {
            topics: Topics::from([event.topic0]),
            data: hex!("")[..].into(),
        };
        assert_eq!(event.decode(&log).unwrap(), ((), ()),);
        assert_eq!(event.encode(&(), &()), log);
    }

    #[test]
    fn anonymous_event_with_indexed_dynamic_field() {
        let anon = AnonymousEventEncoder::<
            (Cow<str>, Cow<[(U256, (bool, Cow<Bytes<[u8]>>))]>),
            (U256, U256),
        >::new();

        let indices = (
            "hello world".into(),
            vec![
                (U256::MAX - 1, (true, Bytes::borrowed(&[1, 2, 3]))),
                (U256::MAX - 2, (true, Bytes::borrowed(&[4, 5, 6]))),
            ]
            .into(),
        );
        let fields = (uint!("1"), uint!("2"));

        let log = Log {
            topics: Topics::from([
                hex!("47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"),
                hex!("6b8a0e75eceddd0e7d4d0413a720bce2cb899061e362357db170c49c5563672f"),
            ]),
            data: hex!(
                "0000000000000000000000000000000000000000000000000000000000000001
                 0000000000000000000000000000000000000000000000000000000000000002"
            )[..]
                .into(),
        };

        assert_eq!(anon.encode(&indices, &fields), log);

        // Note that indexed dynamic fields are **not** actually recoverable.
        assert_eq!(anon.decode(&log).unwrap(), (Default::default(), fields));
    }
}
