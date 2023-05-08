use nom::{branch::alt, character::complete::multispace0, combinator::value, sequence::preceded};
use serde::de::{self, value::BorrowedStrDeserializer, Deserializer as SerdeDeserializer};

use super::Deserializer;
use crate::parse::sgml::element::any_start_tag;
use crate::parse::sgml::element::end_tag;
use crate::{
    error::{Error, Result},
    parse::sgml::element::elem_value,
};

/// Implementor of the serde `VariantAccess` trait for OFX.
pub(super) struct VariantAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
}

impl<'a, 'de, 'h> VariantAccess<'a, 'de, 'h> {
    pub fn new(de: &'a mut Deserializer<'de, 'h>) -> Self {
        VariantAccess { de }
    }
}

impl<'a, 'de, 'h> de::VariantAccess<'de> for VariantAccess<'a, 'de, 'h> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        let name = self.de.consume(preceded(multispace0, any_start_tag))?;
        self.de.consume(preceded(multispace0, end_tag(name)))?;
        Ok(())
    }

    fn newtype_variant_seed<T: de::DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value> {
        seed.deserialize(&mut *self.de)
    }

    fn tuple_variant<V: de::Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value> {
        self.de.deserialize_tuple(len, visitor)
    }

    fn struct_variant<V: de::Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value> {
        self.de.deserialize_map(visitor)
    }
}

/// Implementor of the serde `EnumAccess` trait for OFX.
pub(super) struct EnumAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
}

impl<'a, 'de, 'h> EnumAccess<'a, 'de, 'h> {
    pub fn new(de: &'a mut Deserializer<'de, 'h>) -> Self {
        EnumAccess { de }
    }
}

impl<'a, 'de, 'h> de::EnumAccess<'de> for EnumAccess<'a, 'de, 'h> {
    type Error = Error;
    type Variant = VariantAccess<'a, 'de, 'h>;

    fn variant_seed<V: de::DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, VariantAccess<'a, 'de, 'h>)> {
        let name = self
            .de
            .peek(alt((any_start_tag, value("TODO", elem_value))))
            .unwrap();
        let name = seed.deserialize(BorrowedStrDeserializer::new(name))?;
        Ok((name, VariantAccess::new(self.de)))
    }
}
