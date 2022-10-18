use std::hash::{Hash, Hasher};

use ethabi::{
    ethereum_types::U256,
    token::{LenientTokenizer, Tokenizer},
    Token,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    lex::{expect, take_while, Lex, LexErrorKind, LexResult},
    strict_partial_ord::StrictPartialOrd,
};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
/// ethabi token wrapper
pub struct EthAbiToken {
    /// Inner ethabi token value
    value: Token,
}

impl From<Token> for EthAbiToken {
    fn from(value: Token) -> Self {
        Self::new(value)
    }
}

impl EthAbiToken {
    /// Wraps an ethabi token for wirefilter consumption.
    pub fn new(value: Token) -> Self {
        Self { value }
    }

    /// Returns the underlying token.
    pub fn value(&self) -> &Token {
        &self.value
    }

    /// Parses the token from a string.
    pub fn parse(input: &str) -> Result<Self, EthAbiTokenError> {
        let (ethabi_type, ethabi_value) = input
            .split_once(" ")
            .ok_or_else(|| EthAbiTokenError::NoSeparatorFound)?;

        let param_type = ethabi::param_type::Reader::read(ethabi_type)
            .map_err(|e| EthAbiTokenError::ParseType(e.to_string()))?;

        // special case for catching unknown types for single values, as ethabi would
        // parse any unknown type as an enum
        if param_type == ethabi::ParamType::Uint(8) && !ethabi_type.starts_with("uint8") {
            return Err(EthAbiTokenError::UnknownType);
        }

        let token = LenientTokenizer::tokenize(&param_type, ethabi_value)
            .map_err(|e| EthAbiTokenError::Tokenize(e.to_string()))?;

        Ok(Self::new(token))
    }
}

impl Eq for EthAbiToken {}

impl Hash for EthAbiToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // hash on representation, seems to be the easiest way
        self.value.to_string().hash(state);
    }
}

impl StrictPartialOrd for EthAbiToken {}

impl std::cmp::PartialOrd for EthAbiToken {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use ethabi::Token::*;

        match (self.value(), other.value()) {
            (Uint(a), Uint(b)) => a.partial_cmp(&b),
            // signed 256-bit ints aren't implemented correctly
            (Int(_), Int(_)) => None,
            (String(a), String(b)) => a.partial_cmp(b),
            (Bytes(a), Bytes(b)) => a.partial_cmp(b),
            (Address(a), Address(b)) => a.partial_cmp(b),
            // TODO find a better way?
            (Tuple(a), Tuple(b)) => a
                .iter()
                .map(|v| EthAbiToken::new(v.clone()))
                .partial_cmp(b.iter().map(|v| EthAbiToken::new(v.clone()))),
            (Array(a), Array(b)) => a
                .iter()
                .map(|v| EthAbiToken::new(v.clone()))
                .partial_cmp(b.iter().map(|v| EthAbiToken::new(v.clone()))),
            (FixedArray(a), FixedArray(b)) => a
                .iter()
                .map(|v| EthAbiToken::new(v.clone()))
                .partial_cmp(b.iter().map(|v| EthAbiToken::new(v.clone()))),
            // turn into bytes to bytes comparison
            (Address(a), Bytes(_)) => {
                EthAbiToken::new(Bytes(a.as_bytes().to_vec())).partial_cmp(other)
            }
            (Bytes(_), Address(_)) => other.partial_cmp(self),
            // turn into bytes to uint comparison
            (Address(a), Uint(_)) => {
                EthAbiToken::new(Bytes(a.as_bytes().to_vec())).partial_cmp(other)
            }
            (Uint(_), Address(_)) => other.partial_cmp(self),
            // turn into bytes to bytes comparison
            (Uint(a), Bytes(_)) => {
                let mut buf = [0u8; 32];
                a.to_big_endian(&mut buf);
                EthAbiToken::new(Bytes(buf.to_vec())).partial_cmp(other)
            }
            (Bytes(_), Uint(_)) => other.partial_cmp(self),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum EthAbiTokenError {
    #[error("no double quote found, ethabi value should be put in quotes")]
    NoDoubleQuoteFound,
    #[error("no space separator found, type should precede value")]
    NoSeparatorFound,
    #[error("error parsing type: {0}")]
    ParseType(String),
    #[error("error tokenizing value: {0}")]
    Tokenize(String),
    #[error("unknown type")]
    UnknownType,
    #[error("error parsing u256 integer: {0}")]
    ParseU256(String),
}

fn lex_digits(input: &str) -> LexResult<'_, &str> {
    // Lex any supported digits (up to radix 16) for better error locations.
    take_while(input, "digit", |c| c.is_digit(16))
}

impl<'i> Lex<'i> for EthAbiToken {
    fn lex(input: &str) -> LexResult<'_, Self> {
        if let Ok(input) = expect(input, "\"") {
            let full_input = input;
            let mut res = String::new();
            let mut iter = input.chars();
            loop {
                match iter
                    .next()
                    .ok_or((LexErrorKind::MissingEndingQuote, full_input))?
                {
                    '\\' => {
                        let input = iter.as_str();

                        let c = iter
                            .next()
                            .ok_or((LexErrorKind::MissingEndingQuote, full_input))?;

                        res.push(match c {
                            '"' | '\\' => c,
                            _ => {
                                return Err((
                                    LexErrorKind::InvalidCharacterEscape,
                                    &input[..c.len_utf8()],
                                ));
                            }
                        });
                    }
                    '"' => {
                        return Ok((
                            Self::parse(&res)
                                .map_err(|e| (LexErrorKind::ParseEthAbiToken(e), iter.as_str()))?,
                            iter.as_str(),
                        ))
                    }
                    c => res.push(c),
                };
            }
        } else if let Ok(unprefixed) = expect(input, "0x") {
            let (data, rest) = lex_digits(unprefixed)?;
            let value = match U256::from_str_radix(data, 16) {
                Err(e) => {
                    return Err((
                        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::ParseU256(e.to_string())),
                        rest,
                    ));
                }
                Ok(value) => value,
            };
            Ok((EthAbiToken::new(Token::Uint(value)), rest))
        } else {
            let (data, rest) = lex_digits(input)?;
            let value = match U256::from_str_radix(data, 10) {
                Err(e) => {
                    return Err((
                        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::ParseU256(e.to_string())),
                        rest,
                    ));
                }
                Ok(value) => value,
            };
            Ok((EthAbiToken::new(Token::Uint(value)), rest))
        }
    }
}

#[test]
fn test_ethabi_token() {
    assert_err!(
        EthAbiToken::lex("hello"),
        LexErrorKind::ExpectedName("digit"),
        "hello"
    );
    assert_err!(
        EthAbiToken::lex("0xhello"),
        LexErrorKind::ExpectedName("digit"),
        "hello"
    );
    assert_err!(
        EthAbiToken::lex("\"hello"),
        LexErrorKind::MissingEndingQuote,
        "hello"
    );
    assert_err!(
        EthAbiToken::lex("\"hello\""),
        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::NoSeparatorFound),
        ""
    );
    assert_err!(
        EthAbiToken::lex("\"hello hello\""),
        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::UnknownType),
        ""
    );
    assert_ok!(
        EthAbiToken::lex("\"string hello\""),
        Token::String("hello".into()).into(),
        ""
    );
    assert_err!(
        EthAbiToken::lex("\"address deadbeef\""),
        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::Tokenize("Invalid data".into())),
        ""
    );
    assert_ok!(
        EthAbiToken::lex(
            "\"bytes 69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b\""
        ),
        Token::Bytes(vec![
            105, 251, 197, 202, 6, 121, 57, 113, 10, 107, 115, 12, 37, 114, 9, 84, 185, 25, 93,
            110, 81, 44, 65, 134, 195, 33, 180, 139, 22, 169, 233, 123
        ])
        .into(),
        ""
    );
    assert_ok!(
        EthAbiToken::lex(
            "\"uint 69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b\""
        ),
        Token::Uint("69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b".into())
            .into(),
        ""
    );
    assert_ok!(
        EthAbiToken::lex("\"address 7a250d5630b4cf539739df2c5dacb4c659f2488d\""),
        Token::Address("7a250d5630b4cf539739df2c5dacb4c659f2488d".parse().unwrap()).into(),
        ""
    );
    assert_ok!(
        EthAbiToken::lex(
            "\"(uint) (69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b)\""
        ),
        Token::Tuple(vec![Token::Uint(
            "69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b".into()
        )])
        .into(),
        ""
    );
    assert_err!(
        EthAbiToken::lex(
            "\"(uint) 69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b\""
        ),
        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::Tokenize("Invalid data".into())),
        ""
    );
    assert_ok!(
        EthAbiToken::lex("\"(string,string) (1,\\\"hello\\\")\""),
        Token::Tuple(vec![
            Token::String("1".into()),
            Token::String("\"hello\"".into())
        ])
        .into(),
        ""
    );
    assert_ok!(
        EthAbiToken::lex("\"(uint,bool,string) (1,1,\\\"hello\\\")\""),
        Token::Tuple(vec![
            Token::Uint(1.into()),
            Token::Bool(true),
            Token::String("\"hello\"".into())
        ])
        .into(),
        ""
    );
    assert_ok!(
        EthAbiToken::lex("0x0001x"),
        Token::Uint(1.into()).into(),
        "x"
    );
    assert_ok!(EthAbiToken::lex("1x"), Token::Uint(1.into()).into(), "x");
    assert_err!(
        EthAbiToken::lex(
            "\"(uint) 69fbc5ca067939710a6b730c25720954b9195d6e512c4186c321b48b16a9e97b\""
        ),
        LexErrorKind::ParseEthAbiToken(EthAbiTokenError::Tokenize("Invalid data".into())),
        ""
    );
}
