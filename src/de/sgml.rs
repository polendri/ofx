mod map;
mod seq;
mod sum;

use std::borrow::Cow;

use nom::{
    character::complete::{anychar, i16, i32, i64, i8, multispace0, u16, u32, u64, u8},
    combinator::{map, opt},
    error::{convert_error, Error as BriefError, VerboseError},
    number::complete::{double, float},
    Err, Parser,
};
use serde::Deserialize;
use serde::{
    de::{self, Visitor},
    forward_to_deserialize_any,
};

use self::map::MapAccess;
use self::seq::SeqAccess;
use self::sum::EnumAccess;
use crate::ofx::header::*;
use crate::ofx::Ofx;
use crate::parse::sgml::element::end_tag;
use crate::parse::sgml::{
    element::{elem_value, whitespace_preceded},
    header::ofx_header,
};
use crate::{
    error::{Error, Result},
    parse::sgml::element::start_tag,
};

pub struct Deserializer<'de, 'h> {
    pub header: &'h OfxHeader<'de>,
    input: &'de str,
    skip_next_outer: bool,
}

impl<'de, 'h> Deserializer<'de, 'h> {
    /// Creates an OFX deserializer from a &str.
    pub fn from_str(header: &'h OfxHeader<'de>, input: &'de str) -> Result<Self> {
        Ok(Deserializer {
            header,
            input,
            skip_next_outer: false,
        })
    }

    fn consume<O, P>(&mut self, mut p: P) -> Result<O>
    where
        P: Parser<&'de str, O, VerboseError<&'de str>>,
    {
        p.parse(self.input)
            .map(|(input, v)| {
                self.input = input;
                v
            })
            .map_err(|e| match e {
                Err::Incomplete(_) => Error::ParseIncomplete,
                Err::Error(e) | Err::Failure(e) => Error::ParseError(convert_error(self.input, e)),
            })
    }

    fn peek<O, P>(&self, mut p: P) -> Option<O>
    where
        P: Parser<&'de str, O, BriefError<&'de str>>,
    {
        p.parse(self.input).ok().map(|(_, v)| v)
    }
}

impl<'de, 'h, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de, 'h> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::UnsupportedDataType)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i8(self.consume(whitespace_preceded(i8))?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i16(self.consume(whitespace_preceded(i16))?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i32(self.consume(whitespace_preceded(i32))?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i64(self.consume(whitespace_preceded(i64))?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u8(self.consume(whitespace_preceded(u8))?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u16(self.consume(whitespace_preceded(u16))?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.consume(whitespace_preceded(u32))?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u64(self.consume(whitespace_preceded(u64))?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f32(self.consume(whitespace_preceded(float))?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(self.consume(whitespace_preceded(double))?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_char(self.consume(whitespace_preceded(anychar))?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.consume(elem_value)? {
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
            Cow::Owned(_) => Err(Error::InvalidBorrowedStr),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let v = self.consume(elem_value)?;
        visitor.visit_string(v.into_owned())
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Option is only used for optional fields, the `None` case of which is handled by serde
        visitor.visit_some(self)
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

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(SeqAccess::new(self, None))
    }

    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(SeqAccess::new(self, Some(len)))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MapAccess::new(self, false))
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let skip_next_outer = self.skip_next_outer;
        let flattened = fields.contains(&"$value");

        if !skip_next_outer {
            self.consume(whitespace_preceded(start_tag(name)))?;
        }
        let result = visitor.visit_map(MapAccess::new(self, flattened));
        if !skip_next_outer {
            if flattened {
                self.consume(opt(whitespace_preceded(end_tag(name))))?;
            } else {
                self.consume(whitespace_preceded(end_tag(name)))?;
            }
        }
        self.skip_next_outer = false;
        result
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
        visitor.visit_enum(EnumAccess::new(self))
    }

    forward_to_deserialize_any! { bool bytes byte_buf identifier ignored_any }
}

pub fn from_str<'a, T>(s: &'a str) -> Result<Ofx<'a, T>>
where
    T: Deserialize<'a>,
{
    let (s, header) = ofx_header::<VerboseError<&str>>(s).map_err(|e| match e {
        Err::Incomplete(_) => Error::ParseIncomplete,
        Err::Error(e) | Err::Failure(e) => Error::ParseError(convert_error(s, e)),
    })?;
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

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(rename = "STRUCT")]
    struct SingleFieldStruct<'a> {
        #[serde(rename = "FIELD")]
        field: &'a str,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(rename = "OSTRUCT")]
    struct NestedStruct<'a> {
        #[serde(rename = "OFIELD", borrow)]
        ofield: SingleFieldStruct<'a>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(rename = "STRUCT")]
    struct SingleValueFieldStruct {
        #[serde(rename = "$value")]
        field: i32,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(rename = "STRUCT")]
    struct MultiFieldStruct<'a> {
        #[serde(rename = "FIELD1")]
        field1: &'a str,
        #[serde(rename = "FIELD2")]
        field2: Option<&'a str>,
        #[serde(rename = "FIELD3")]
        field3: Option<i32>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct MultiValueFieldStruct<'a> {
        #[serde(borrow, rename = "SINGLE")]
        single: SingleFieldStruct<'a>,
        #[serde(rename = "$value")]
        multi: Vec<SingleFieldStruct<'a>>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    enum Enum {
        #[serde(rename = "UNIT")]
        Unit,
        #[serde(rename = "NEWT")]
        Newtype(i32),
        #[serde(rename = "TUPLE")]
        Tuple(i32, SingleValueFieldStruct),
        #[serde(rename = "STRUCT")]
        Struct {
            #[serde(rename = "FIELD1")]
            field1: i32,
            #[serde(rename = "FIELD2")]
            field2: i32,
        },
    }

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

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__i8(input: &str, expected: Result<i8>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__i16(input: &str, expected: Result<i16>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__i32(input: &str, expected: Result<i32>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__i64(input: &str, expected: Result<i64>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__u8(input: &str, expected: Result<u8>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__u16(input: &str, expected: Result<u16>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__u32(input: &str, expected: Result<u32>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"     , Ok(123), ""  ; "ok"        )]
    #[test_case("\r\n123 ", Ok(123), " " ; "whitespace")]
    fn deserializer__u64(input: &str, expected: Result<u64>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123.4"     , Ok(123.4), ""  ; "ok"        )]
    #[test_case("\r\n123.4 ", Ok(123.4), " " ; "whitespace")]
    fn deserializer__f32(input: &str, expected: Result<f32>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123.4"     , Ok(123.4), ""  ; "ok"        )]
    #[test_case("\r\n123.4 ", Ok(123.4), " " ; "whitespace")]
    fn deserializer__f64(input: &str, expected: Result<f64>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123", Ok('1'), "23" ; "ok")]
    #[test_case(" \r\n123", Ok('1'), "23" ; "whitespace")]
    fn deserializer__char(input: &str, expected: Result<char>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(""           , Ok("")                        , ""       ; "eof"                 )]
    #[test_case("</TAG>"     , Ok("")                        , "</TAG>" ; "end tag"             )]
    #[test_case(" \r\nvalue ", Ok(" \r\nvalue ")             , ""       ; "whitespace"          )]
    #[test_case("a1A!&CDATA&", Ok("a1A!&CDATA&")             , ""       ; "no escape sequences" )]
    #[test_case("a1&lt;A!"   , Err(Error::InvalidBorrowedStr), ""       ; "with escape sequence")]
    fn deserializer__str(input: &str, expected: Result<&str>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(""           , Ok(String::from(""))           , ""       ; "eof"                 )]
    #[test_case("</TAG>"     , Ok(String::from(""))           , "</TAG>" ; "end tag"             )]
    #[test_case("\r\nvalue " , Ok(String::from("\r\nvalue ")) , ""       ; "whitespace"          )]
    #[test_case("a1A!&CDATA&", Ok(String::from("a1A!&CDATA&")), ""       ; "no escape sequences" )]
    #[test_case("a1&lt;A!"   , Ok(String::from("a1<A!"))      , ""       ; "with escape sequence")]
    fn deserializer__string(input: &str, expected: Result<String>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("123"      , Ok(Some(123)), "" ; "ok")]
    fn deserializer__option(input: &str, expected: Result<Option<i32>>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("", Ok(vec![]), "" ; "eof")]
    #[test_case(
        "<STRUCT>123",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        "" ;
        "single unterminated elem then eof"
    )]
    #[test_case(
        " \r\n <STRUCT>123",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        "" ;
        "single unterminated elem with whitespace then eof"
    )]
    #[test_case(
        "<STRUCT>123</END>",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        "</END>" ;
        "single unterminated elem then end tag"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT>",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        "" ;
        "single terminated elem then eof"
    )]
    #[test_case(
        " \r\n <STRUCT>123  </STRUCT> ",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        " " ;
        "single terminated elem with whitespace then eof"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT></END>",
        Ok(vec![SingleValueFieldStruct { field: 123 }]),
        "</END>" ;
        "single terminated elem then end tag"
    )]
    #[test_case(
        "<STRUCT>123<STRUCT>456",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "" ;
        "multi unterminated elems then eof"
    )]
    #[test_case(
        " \r\n <STRUCT>123\r\n<STRUCT>456",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "" ;
        "multi unterminated elems with whitespace then eof"
    )]
    #[test_case(
        "<STRUCT>123<STRUCT>456</END>",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "</END>" ;
        "multi unterminated elems then end tag"
    )]
    #[test_case(
        " \r\n <STRUCT>123</STRUCT>\r\n<STRUCT>456</STRUCT>",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "" ;
        "multi terminated elems with whitespace then eof"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT><STRUCT>456</STRUCT>",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "" ;
        "multi terminated elems then eof"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT><STRUCT>456</STRUCT></END>",
        Ok(vec![SingleValueFieldStruct { field: 123 }, SingleValueFieldStruct { field: 456 }]),
        "</END>" ;
        "multi terminated elems then end tag"
    )]
    fn deserializer__seq(
        input: &str,
        expected: Result<Vec<SingleValueFieldStruct>>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("", Ok(BTreeMap::new()), "" ; "eof")]
    #[test_case(
        "<NAME>123",
        Ok(BTreeMap::from([("NAME", 123)])),
        "" ;
        "unterminated elem then eof"
    )]
    #[test_case(
        "<NAME>123</FOO>",
        Ok(BTreeMap::from([("NAME", 123)])),
        "</FOO>" ;
        "unterminated elem then end tag"
    )]
    #[test_case(
        "<NAME>123</NAME>",
        Ok(BTreeMap::from([("NAME", 123)])),
        "" ;
        "terminated elem then eof"
    )]
    #[test_case(
        "<NAME>123</NAME></FOO>",
        Ok(BTreeMap::from([("NAME", 123)])),
        "</FOO>" ;
        "terminated elem then end tag"
    )]
    #[test_case(
        "<NAME1>123<NAME2>456</NAME2>",
        Ok(BTreeMap::from([("NAME1", 123), ("NAME2", 456)])),
        "" ;
        "pair of elems"
    )]
    fn deserializer__map(
        input: &str,
        expected: Result<BTreeMap<&'static str, i32>>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(
        "<STRUCT><FIELD>value</STRUCT></END>",
        Ok(SingleFieldStruct { field: "value" }),
        "</END>" ;
        "unterminated field then end tag"
    )]
    #[test_case(
        "<STRUCT><FIELD>value</FIELD></STRUCT>",
        Ok(SingleFieldStruct { field: "value" }),
        "" ;
        "terminated field then eof"
    )]
    fn deserializer__struct__single_field(
        input: &str,
        expected: Result<SingleFieldStruct>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(
        "<OSTRUCT><OFIELD><FIELD>value</OFIELD></OSTRUCT>",
        Ok(NestedStruct {
            ofield: SingleFieldStruct { field: "value" },
        }),
        "" ;
        "single nested struct then eof"
    )]
    fn deserializer__struct__nested_struct(
        input: &str,
        expected: Result<NestedStruct>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(
        "<STRUCT>123",
        Ok(SingleValueFieldStruct { field: 123 }),
        "" ;
        "unterminated then eof"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT>",
        Ok(SingleValueFieldStruct { field: 123 }),
        "" ;
        "terminated then eof"
    )]
    #[test_case(
        "<STRUCT>123</END>",
        Ok(SingleValueFieldStruct { field: 123 }),
        "</END>" ;
        "unterminated then end tag"
    )]
    #[test_case(
        "<STRUCT>123</STRUCT></END>",
        Ok(SingleValueFieldStruct { field: 123 }),
        "</END>" ;
        "terminated then end tag"
    )]
    #[test_case(
        " \r\n <STRUCT>123 ",
        Ok(SingleValueFieldStruct { field: 123 }),
        " " ;
        "unterminated with whitespace"
    )]
    #[test_case(
        " \r\n <STRUCT>123  </STRUCT> ",
        Ok(SingleValueFieldStruct { field: 123 }),
        " " ;
        "terminated with whitespace"
    )]
    fn deserializer__single_value_field_struct(
        input: &str,
        expected: Result<SingleValueFieldStruct>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case(
        "<STRUCT><FIELD1>v1<FIELD2>v2</FIELD2><FIELD3>123</STRUCT>",
        Ok(MultiFieldStruct { field1: "v1", field2: Some("v2"), field3: Some(123) }),
        "" ;
        "all fields then eof"
    )]
    #[test_case(
        "<STRUCT><FIELD1>v1<FIELD2>v2</FIELD2><FIELD3>123</STRUCT></END>",
        Ok(MultiFieldStruct { field1: "v1", field2: Some("v2"), field3: Some(123) }),
        "</END>" ;
        "all fields then end tag"
    )]
    #[test_case(
        "<STRUCT><FIELD3>123<FIELD1>v1</FIELD1><FIELD2>v2</FIELD2></STRUCT>",
        Ok(MultiFieldStruct { field1: "v1", field2: Some("v2"), field3: Some(123) }),
        "" ;
        "all elems out of order"
    )]
    #[test_case(
        "<STRUCT><FIELD1>v1</STRUCT>",
        Ok(MultiFieldStruct { field1: "v1", field2: None, field3: None }),
        "" ;
        "optional elems missing"
    )]
    fn deserializer__struct__multi_field(
        input: &str,
        expected: Result<MultiFieldStruct>,
        remaining: &str,
    ) {
        assert_deserialize(input, expected, remaining);
    }

    #[test_case("UNIT"          , Ok(Enum::Unit)      , "" ; "text unit variant"            )]
    #[test_case("<UNIT>"        , Ok(Enum::Unit)      , "" ; "unterminated tag unit variant")]
    #[test_case("<UNIT></UNIT>" , Ok(Enum::Unit)      , "" ; "terminated tag unit variant"  )]
    #[test_case("<NEWT>3"       , Ok(Enum::Newtype(3)), "" ; "unterminated newtype variant" )]
    #[test_case("<NEWT>3</NEWT>", Ok(Enum::Newtype(3)), "" ; "terminated newtype variant"   )]
    #[test_case(
        "<TUPLE>3<STRUCT>4</STRUCT></TUPLE>",
        Ok(Enum::Tuple(3, SingleValueFieldStruct { field: 4 })),
        "" ;
        "tuple variant"
    )]
    #[test_case(
        "<STRUCT><FIELD1>1</FIELD1><FIELD2>2</STRUCT>",
        Ok(Enum::Struct { field1: 1, field2: 2 }),
        "" ;
        "struct variant"
    )]
    fn deserializer__enum(input: &str, expected: Result<Enum>, remaining: &str) {
        assert_deserialize(input, expected, remaining);
    }
}
