use nom::{branch::alt, combinator::eof};
use serde::de::{self, DeserializeSeed};

use super::Deserializer;
use crate::error::{Error, Result};
use crate::parse::sgml::element::{any_end_tag, whitespace_preceded};

pub(super) struct SeqAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
    len: Option<usize>,
}

impl<'a, 'de, 'h> SeqAccess<'a, 'de, 'h> {
    pub(super) fn new(de: &'a mut Deserializer<'de, 'h>, len: Option<usize>) -> Self {
        SeqAccess { de, len }
    }
}

impl<'a, 'de, 'h> de::SeqAccess<'de> for SeqAccess<'a, 'de, 'h> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(0) = self.len {
            return Ok(None);
        }

        if self
            .de
            .peek(whitespace_preceded(alt((any_end_tag, eof))))
            .is_some()
        {
            match self.len {
                Some(n) if n > 0 => Err(Error::InvalidTupleLength),
                _ => Ok(None),
            }
        } else {
            self.len = self.len.map(|n| n - 1);
            seed.deserialize(&mut *self.de).map(Some)
        }
    }
}
