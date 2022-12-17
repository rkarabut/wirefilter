use crate::{
    lex::{take_while, Lex, LexErrorKind, LexResult},
    strict_partial_ord::StrictPartialOrd,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize, Serialize)]
#[serde(transparent)]
/// Bytes literal represented either by raw bytes or a hexadecimal string.
pub struct HexString {
    data: Vec<u8>,
}

impl HexString {
    /// Creates a new hexstring from an array of bytes.
    pub fn new(data: Vec<u8>) -> Self {
        Self { data }
    }

    /// Returns the underlying data.
    pub fn as_bytes(&self) -> &[u8] {
        self.data.as_ref()
    }
}

fn lex_digits(input: &str) -> LexResult<'_, &str> {
    // Lex any supported digits (up to radix 16) for better error locations.
    take_while(input, "digit", |c| c.is_ascii_hexdigit())
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
