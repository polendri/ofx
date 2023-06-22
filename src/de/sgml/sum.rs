use std::borrow::Cow;

use nom::combinator::opt;
use nom::{branch::alt, combinator::map};
use serde::de::{self, value::BorrowedStrDeserializer, Deserializer as SerdeDeserializer};

use super::Deserializer;
use crate::parse::sgml::element::{any_start_tag, end_tag, whitespace_preceded};
use crate::{
    error::{Error, Result},
    parse::sgml::element::elem_value,
};

/// Implementor of the serde `VariantAccess` trait for OFX.
pub(super) struct VariantAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
    name: &'de str,
}

impl<'a, 'de, 'h> VariantAccess<'a, 'de, 'h> {
    pub(super) fn new(de: &'a mut Deserializer<'de, 'h>, name: &'de str) -> Self {
        VariantAccess { de, name }
    }
}

impl<'a, 'de, 'h> de::VariantAccess<'de> for VariantAccess<'a, 'de, 'h> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        self.de
            .consume(opt(whitespace_preceded(end_tag(self.name))))?;
        Ok(())
    }

    fn newtype_variant_seed<T: de::DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value> {
        let result = seed.deserialize(&mut *self.de);
        self.de
            .consume(opt(whitespace_preceded(end_tag(self.name))))?;
        result
    }

    fn tuple_variant<V: de::Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value> {
        let result = self.de.deserialize_tuple(len, visitor);
        self.de.consume(whitespace_preceded(end_tag(self.name)))?;
        result
    }

    fn struct_variant<V: de::Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value> {
        let result = self.de.deserialize_map(visitor);
        self.de.consume(whitespace_preceded(end_tag(self.name)))?;
        result
    }
}

/// Implementor of the serde `EnumAccess` trait for OFX.
pub(super) struct EnumAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
}

impl<'a, 'de, 'h> EnumAccess<'a, 'de, 'h> {
    pub(super) fn new(de: &'a mut Deserializer<'de, 'h>) -> Self {
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
        let name_str = match self.de.consume(whitespace_preceded(alt((
            map(any_start_tag, Cow::Borrowed),
            elem_value,
        ))))? {
            Cow::Borrowed(name) => Ok(name.trim_end()),
            _ => Err(Error::EscapesInEnumVariant),
        }?;

        let name = seed.deserialize(BorrowedStrDeserializer::new(name_str))?;
        Ok((name, VariantAccess::new(self.de, name_str)))
    }
}
