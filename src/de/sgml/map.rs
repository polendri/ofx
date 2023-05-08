use nom::{character::complete::multispace0, combinator::opt, sequence::tuple};
use serde::de::{self, value::BorrowedStrDeserializer, DeserializeSeed};

use super::Deserializer;
use crate::error::{Error, Result};
use crate::parse::sgml::element::end_tag;
use crate::parse::sgml::element::{any_end_tag, any_start_tag, whitespace_delimited};

/// Implementor of the serde `MapAccess` trait for OFX.
pub(super) struct MapAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
    /// The end tag name to look for after deserializing a value
    tag: Option<&'de str>,
    /// Whether this `MapAccess` is to deserialize all inner contents of an outer element
    strip_outer: bool,
}

impl<'a, 'de, 'h> MapAccess<'a, 'de, 'h> {
    pub fn new(de: &'a mut Deserializer<'de, 'h>, strip_outer: bool) -> Self {
        MapAccess {
            de,
            tag: None,
            strip_outer,
        }
    }
}

impl<'a, 'de, 'h> de::MapAccess<'de> for MapAccess<'a, 'de, 'h> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        debug_assert!(self.strip_outer || self.tag.is_none());

        if self.de.peek(whitespace_delimited(any_end_tag)).is_some() {
            return Ok(None);
        }

        if self.strip_outer {
            if self.tag.is_some() {
                Ok(None)
            } else {
                self.tag = Some("$value");
                seed.deserialize(BorrowedStrDeserializer::new("$value"))
                    .map(Some)
            }
        } else {
            match self.de.consume(any_start_tag) {
                Ok(name) => {
                    self.tag = Some(name);
                    seed.deserialize(BorrowedStrDeserializer::new(name))
                        .map(Some)
                }
                Err(_) => Ok(None),
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        debug_assert!(self.tag.is_some());

        self.de.skip_next_outer = true;
        let value = seed.deserialize(&mut *self.de)?;
        self.de.skip_next_outer = false;

        if !self.strip_outer {
            let tag = self.tag.take().unwrap();
            self.de.consume(tuple((multispace0, opt(end_tag(tag)))))?;
        }

        Ok(value)
    }
}
