use nom::{branch::alt, combinator::eof};
use serde::de::{self, DeserializeSeed};

use super::Deserializer;
use crate::error::{Error, Result};
use crate::parse::{
    sgml::element::{any_end_tag, whitespace_delimited},
    use_nom_opt,
};

pub(super) struct SeqAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
}

impl<'a, 'de, 'h> SeqAccess<'a, 'de, 'h> {
    pub fn new(de: &'a mut Deserializer<'de, 'h>) -> Self {
        SeqAccess { de }
    }
}

impl<'a, 'de, 'h> de::SeqAccess<'de> for SeqAccess<'a, 'de, 'h> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if use_nom_opt(whitespace_delimited(alt((any_end_tag, eof))), self.de.input).is_some() {
            Ok(None)
        } else {
            seed.deserialize(&mut *self.de).map(Some)
        }
    }
}
