use crate::{
    lex::{take_while, Lex, LexErrorKind, LexResult},
    strict_partial_ord::StrictPartialOrd,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(transparent)]
pub struct HexString {
    pub data: Vec<u8>,
}

fn lex_digits(input: &str) -> LexResult<'_, &str> {
    // Lex any supported digits (up to radix 16) for better error locations.
    take_while(input, "digit", |c| c.is_digit(16))
}

impl<'i> Lex<'i> for HexString {
    fn lex(input: &str) -> LexResult<'_, Self> {
        let (data, rest) = lex_digits(input.strip_prefix("0x").unwrap_or(input))?;

        match hex::decode(data) {
            Ok(data) => Ok((Self { data }, rest)),
            Err(e) => Err((LexErrorKind::ParseHexString(e), rest)),
        }
    }
}

impl StrictPartialOrd for HexString {}

impl std::cmp::PartialOrd for HexString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.data.cmp(&other.data))
    }
}
