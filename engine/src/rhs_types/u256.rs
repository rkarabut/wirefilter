use crate::{
    lex::{expect, span, take_while, Lex, LexErrorKind, LexResult},
    strict_partial_ord::StrictPartialOrd,
};
use bigint::uint::U256;
use serde::{de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
use std::ops::RangeInclusive;
use thiserror::Error;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
/// 256-bit unsigned integer literal wrapper
pub struct U256Wrapper {
    /// Inner 256-bit unsigned integer value
    pub value: bigint::uint::U256,
}

impl From<U256> for U256Wrapper {
    fn from(value: U256) -> Self {
        Self { value }
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum U256Error {
    ParseHex(#[source] hex::FromHexError),
    ParseDec(String),
}

impl Serialize for U256Wrapper {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut data: [u8; 32] = [0; 32];
        self.value.to_big_endian(&mut data);
        serializer.serialize_bytes(&data)
    }
}

struct U256Visitor;

impl<'de> Visitor<'de> for U256Visitor {
    type Value = U256Wrapper;

    fn expecting(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("an integer between 0 and 2^256")
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value.len() > 32 {
            Err(E::custom(format!(
                "u256 out of range, too many bytes supplied: {}",
                value.len()
            )))
        } else {
            Ok(U256Wrapper {
                value: U256::from_big_endian(&value[..32]),
            })
        }
    }
}

impl<'de> Deserialize<'de> for U256Wrapper {
    fn deserialize<D>(deserializer: D) -> Result<U256Wrapper, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_bytes(U256Visitor)
    }
}

impl std::fmt::Display for U256Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseHex(e) => write!(f, "{}", e),
            Self::ParseDec(s) => write!(f, "{}", s),
        }
    }
}

fn lex_digits(input: &str) -> LexResult<'_, &str> {
    // Lex any supported digits (up to radix 16) for better error locations.
    take_while(input, "digit", |c| c.is_ascii_hexdigit())
}

impl<'i> Lex<'i> for U256Wrapper {
    fn lex(input: &str) -> LexResult<'_, Self> {
        if let Ok(input) = expect(input, "0x") {
            let (data, rest) = lex_digits(input)?;
            let mut padded = String::new();
            let maybe_padded = {
                if data.len() % 2 != 0 {
                    padded.push('0');
                    padded.push_str(data);
                    &padded
                } else {
                    data
                }
            };
            match hex::decode(maybe_padded) {
                Err(e) => Err((LexErrorKind::ParseU256(U256Error::ParseHex(e)), rest)),
                Ok(data) => Ok((
                    U256Wrapper {
                        value: U256::from_big_endian(&data),
                    },
                    rest,
                )),
            }
        } else {
            let (data, rest) = lex_digits(input)?;
            match U256::from_dec_str(data) {
                Err(e) => Err((
                    LexErrorKind::ParseU256(U256Error::ParseDec(format!("{:?}", e))),
                    rest,
                )),
                Ok(value) => Ok((U256Wrapper { value }, rest)),
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(transparent)]
pub struct U256Range(RangeInclusive<U256Wrapper>);

impl From<U256Wrapper> for U256Range {
    fn from(i: U256Wrapper) -> Self {
        Self(i..=i)
    }
}

impl From<U256> for U256Range {
    fn from(i: U256) -> Self {
        Self(U256Wrapper { value: i }..=U256Wrapper { value: i })
    }
}

impl From<RangeInclusive<U256>> for U256Range {
    fn from(r: RangeInclusive<U256>) -> Self {
        U256Range(RangeInclusive::new(
            U256Wrapper { value: *r.start() },
            U256Wrapper { value: *r.end() },
        ))
    }
}

impl From<U256Range> for RangeInclusive<U256Wrapper> {
    fn from(range: U256Range) -> Self {
        range.0
    }
}

impl<'i> Lex<'i> for U256Range {
    fn lex(input: &str) -> LexResult<'_, Self> {
        let initial_input = input;
        let (first, input) = U256Wrapper::lex(input)?;
        let (last, input) = if let Ok(input) = expect(input, "..") {
            U256Wrapper::lex(input)?
        } else {
            (first, input)
        };
        if last < first {
            return Err((
                LexErrorKind::IncompatibleRangeBounds,
                span(initial_input, input),
            ));
        }
        Ok(((first.value..=last.value).into(), input))
    }
}

impl PartialEq<U256Wrapper> for U256 {
    fn eq(&self, other: &U256Wrapper) -> bool {
        self.eq(&other.value)
    }
}

impl PartialOrd<U256Wrapper> for U256 {
    fn partial_cmp(&self, other: &U256Wrapper) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other.value))
    }
}

impl StrictPartialOrd for U256Wrapper {}

#[test]
fn test() {
    assert_ok!(U256Wrapper::lex("0"), U256::from(0i32).into(), "");
    assert_ok!(U256Wrapper::lex("0-"), U256::from(0i32).into(), "-");
    assert_ok!(U256Wrapper::lex("0x1f5+"), U256::from(501i32).into(), "+");
    assert_ok!(U256Wrapper::lex("78!"), U256::from(78i32).into(), "!");
    assert_ok!(U256Wrapper::lex("0xefg"), U256::from(239i32).into(), "g");
    assert_err!(
        U256Wrapper::lex("21474836480000000000000000000000000000000000000000000000000000000000000000000000000!"),
        LexErrorKind::ParseU256(U256Error::ParseDec(
            format!("{:?}", U256::from_dec_str("21474836480000000000000000000000000000000000000000000000000000000000000000000000000")
                .unwrap_err())
        )),
        "!"
    );
    assert_err!(
        U256Wrapper::lex("10fex"),
        LexErrorKind::ParseU256(U256Error::ParseDec(format!(
            "{:?}",
            U256::from_dec_str("10fe").unwrap_err()
        ))),
        "x"
    );
    assert_ok!(U256Range::lex("78!"), U256::from(78i32).into(), "!");
    assert_ok!(
        U256Range::lex("0..10"),
        (U256::from(0i32)..=U256::from(10i32)).into()
    );
    assert_ok!(
        U256Range::lex("83..0xefg"),
        (U256::from(83i32)..=U256::from(239i32)).into(),
        "g"
    );
    assert_err!(
        U256Range::lex("10..0"),
        LexErrorKind::IncompatibleRangeBounds,
        "10..0"
    );
}
