use std::iter::repeat_with;

use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_a, is_not, tag, take},
    character::complete::{
        i16, i32, i64, i8, line_ending, multispace0, not_line_ending, u16, u32, u64, u8,
    },
    combinator::{eof, map, opt, peek, recognize, value, verify},
    error::{Error as BriefError, ParseError, VerboseError},
    multi::{many0, many0_count},
    number::complete::{double, float},
    sequence::{delimited, terminated, tuple},
    IResult as NomIResult, Parser,
};

use crate::parse::{
    sgml::{
        element::{any_end_tag, any_start_tag, elem_value},
        header::ofx_header,
    },
    wrap_nom,
};
use crate::{ofx, parse::sgml::element::end_tag};
use crate::{ofx::header::*, parse::sgml::element::any_elem};

use serde::de::{
    self,
    value::{BorrowedStrDeserializer, MapDeserializer, SeqDeserializer},
    DeserializeSeed, EnumAccess, IntoDeserializer, SeqAccess, VariantAccess, Visitor,
};
use serde::Deserialize;

use crate::error::{Error, Result};
use crate::ofx::Ofx;

type IResult<I, O> = NomIResult<I, O, VerboseError<I>>;

struct MapAccess<'a, 'de: 'a, 'h: 'a> {
    de: &'a mut Deserializer<'de, 'h>,
    tag: Option<&'de str>,
}

impl<'a, 'de, 'h> MapAccess<'a, 'de, 'h> {
    fn new(de: &'a mut Deserializer<'de, 'h>) -> Self {
        MapAccess { de, tag: None }
    }
}

impl<'a, 'de, 'h> de::MapAccess<'de> for MapAccess<'a, 'de, 'h> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        debug_assert!(self.tag.is_none());

        (self.de.input, _) = wrap_nom(multispace0, self.de.input)?;

        if alt::<_, _, VerboseError<&str>, _>((tag("</"), eof))(self.de.input).is_ok() {
            return Ok(None);
        }

        match any_start_tag::<BriefError<&str>>(self.de.input) {
            Ok((input, name)) => {
                self.de.input = input;
                self.tag = Some(name);
                seed.deserialize(BorrowedStrDeserializer::new(name))
                    .map(Some)
            }
            Err(_) => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        debug_assert!(self.tag.is_some());

        let value = seed.deserialize(&mut *self.de)?;

        let tag = self.tag.unwrap();
        (self.de.input, _) = wrap_nom(tuple((multispace0, opt(end_tag(tag)))), self.de.input)?;
        self.tag = None;

        Ok(value)
    }
}

pub struct Deserializer<'de, 'h> {
    pub header: &'h OfxHeader<'de>,
    input: &'de str,
}

impl<'de, 'h> Deserializer<'de, 'h> {
    /// Creates an OFX deserializer from a &str.
    pub fn from_str(header: &'h OfxHeader<'de>, input: &'de str) -> Result<Self> {
        Ok(Deserializer { header, input })
    }
}

impl<'de, 'h, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de, 'h> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, i8, multispace0), self.input)?;
        self.input = input;
        visitor.visit_i8(v)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, i16, multispace0), self.input)?;
        self.input = input;
        visitor.visit_i16(v)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, i32, multispace0), self.input)?;
        self.input = input;
        visitor.visit_i32(v)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, i64, multispace0), self.input)?;
        self.input = input;
        visitor.visit_i64(v)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, u8, multispace0), self.input)?;
        self.input = input;
        visitor.visit_u8(v)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, u16, multispace0), self.input)?;
        self.input = input;
        visitor.visit_u16(v)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, u32, multispace0), self.input)?;
        self.input = input;
        visitor.visit_u32(v)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, u64, multispace0), self.input)?;
        self.input = input;
        visitor.visit_u64(v)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, float, multispace0), self.input)?;
        self.input = input;
        visitor.visit_f32(v)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(delimited(multispace0, double, multispace0), self.input)?;
        self.input = input;
        visitor.visit_f64(v)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(take(1u8), self.input)?;
        self.input = input;
        visitor.visit_char(v.chars().next().unwrap())
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (input, v) = wrap_nom(is_not("<>"), self.input)?;
        self.input = input;
        visitor.visit_borrowed_str(v)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match peek::<_, _, VerboseError<&str>, _>(verify(elem_value, |o: &Vec<&str>| {
            o.iter().filter(|v| !v.trim().is_empty()).count() > 0
        }))(self.input)
        {
            Ok(_) => visitor.visit_some(self),
            Err(_) => visitor.visit_none(),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapAccess::new(self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }
}

pub fn from_str<'a, T>(s: &'a str) -> Result<Ofx<'a, T>>
where
    T: Deserialize<'a>,
{
    let (s, header) = wrap_nom(ofx_header, s)?;
    let mut deserializer = Deserializer::from_str(&header, s)?;
    let ofx = T::deserialize(&mut deserializer)?;

    if deserializer.input.is_empty() {
        Ok(Ofx { header, ofx })
    } else {
        Err(Error::TrailingInput)
    }
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use test_case::test_case;

    use super::*;

    const HEADER: OfxHeader = OfxHeader {
        header_version: 100,
        data: OfxContentType::OfxSgml,
        version: 102,
        security: OfxSecurity::None,
        encoding: OfxEncoding::UsAscii,
        charset: OfxCharset::Latin1,
        compression: "",
        old_file_uid: "NONE",
        new_file_uid: "NONE",
    };

    fn assert_deserialize<'de, T>(input: &'de str, expected: Result<T>, remaining: &'de str)
    where
        T: std::fmt::Debug + Deserialize<'de> + PartialEq,
    {
        let mut deserializer = Deserializer::from_str(&HEADER, input).unwrap();
        let result = T::deserialize(&mut deserializer);

        assert_eq!(result, expected);
        assert_eq!(deserializer.input, remaining);
    }

    // TODO: Tests for remaining primitive types
    #[test_case("123", Ok(123), "" ; "ok")]
    #[test_case("abc", Err(Error::ParseError(String::from("0: at line 1, in Digit:\nabc\n^\n\n"))), "abc" ; "err")]
    fn deserializer__i8(input: &str, expected: Result<i8>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"    , Ok(Some(123)), ""        ; "value"             )]
    #[test_case(""       , Ok(None)     , ""        ; "eof"               )]
    #[test_case("\t \r\n", Ok(None)     , "\t \r\n" ; "whitespace"        )]
    #[test_case("<"      , Ok(None)     , "<"       ; "left angle bracket")]
    fn deserializer__option(input: &str, expected: Result<Option<i32>>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("", Ok(BTreeMap::new()), "" ; "eof")]
    #[test_case("<NAME>123", Ok(BTreeMap::from([("NAME", 123)])), "" ; "unterminated elem then eof")]
    #[test_case("<NAME>123</FOO>", Ok(BTreeMap::from([("NAME", 123)])), "</FOO>" ; "unterminated elem then end tag")]
    #[test_case("<NAME>123</NAME>", Ok(BTreeMap::from([("NAME", 123)])), "" ; "terminated elem then eof")]
    #[test_case("<NAME>123</NAME></FOO>", Ok(BTreeMap::from([("NAME", 123)])), "</FOO>" ; "terminated elem then end tag")]
    #[test_case("<NAME1>123<NAME2>456</NAME2>", Ok(BTreeMap::from([("NAME1", 123), ("NAME2", 456)])), "" ; "pair of elems")]
    fn deserializer__map(
        input: &str,
        expected: Result<BTreeMap<&'static str, i32>>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }
}
