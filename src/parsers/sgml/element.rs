//! Parsers for OFX SGML elements.

use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take, take_until},
    character::complete::multispace0,
    combinator::{map, not, opt, peek, recognize, value},
    error::ParseError,
    multi::{many0, many0_count},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

/// Parses text devoid of special characters.
fn normal_chars<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    map(opt(is_not("<&")), |v| v.unwrap_or_default())(input)
}

/// Parses text devoid of special characters.
fn normal_chars1<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    is_not("<&")(input)
}

/// Parses the name of a tag.
fn tag_name<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    recognize(tuple((
        is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        many0(is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")),
    )))(input)
}

/// Parses the start tag of an element.
fn any_start_tag<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("<"), tag_name, tag(">"))(input)
}

/// Parses a named start tag of an element.
fn start_tag<'a, E>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("<"), tag(name), tag(">"))
}

/// Parses the end tag of an element.
fn any_end_tag<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("</"), tag_name, tag(">"))(input)
}

/// Parses a named end tag of an element.
fn end_tag<'a, E>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("</"), tag(name), tag(">"))
}

/// Parses the value of an element.
fn elem_value<'a, E>(input: &'a str) -> IResult<&'a str, Cow<'a, str>, E>
where
    E: ParseError<&'a str>,
{
    let (input, chunks) = many0(alt((
        normal_chars1,
        value("<", tag("&lt;")),
        value(">", tag("&gt;")),
        value("&", tag("&amp;")),
        value(" ", tag("&nbsp;")),
        delimited(tag("<![CDATA["), take_until("]]>"), tag("]]>")),
        tag("&"),
    )))(input)?;

    Ok((
        input,
        match chunks.len() {
            0 => Cow::Borrowed(""),
            1 => Cow::Borrowed(chunks[0]),
            _ => Cow::Owned(chunks.concat()),
        },
    ))
}

/// Parses an element that has no end tag and a text value within it.
///
/// Trims whitespace before running the provided parser for the contents.
pub fn any_value_elem<'a, O, E, P>(
    mut p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, (&str, O), E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, name) = terminated(any_start_tag, multispace0)(input)?;
        let (input, value) = p.parse(input)?;
        Ok((input, (name, value)))
    }
}

/// Parses a named element that has no end tag and a text value within it.
///
/// Trims whitespace before running the provided parser for the contents.
pub fn value_elem<'a, O, E, P>(name: &'a str, p: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(tuple((start_tag(name), multispace0)), p)
}

/// Parses an element that has an end tag and other elements within it.
///
/// Trims whitespace before/after running the provided parser for the contents.
pub fn any_group_elem<'a, O, E, P>(
    mut p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, (&'a str, O), E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, name) = terminated(any_start_tag, multispace0)(input)?;
        let (input, value) = p.parse(input)?;
        let (input, _) = tuple((multispace0, end_tag(name)))(input)?;
        Ok((input, (name, value)))
    }
}

/// Parses a named element that has an end tag and other elements within it.
///
/// Trims whitespace before/after running the provided parser for the contents.
pub fn group_elem<'a, O, E, P>(name: &'a str, p: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    delimited(
        tuple((start_tag(name), multispace0)),
        p,
        tuple((multispace0, end_tag(name))),
    )
}

/// Parses an unknown element into a `(name, value)` pair.
///
/// Since the type of element defines whether it has an end tag in OFX, this necessarily has to take
/// a guess. If the start tag is followed by any non-whitespace text before the next element, then
/// it is assumed not to have an end tag.
pub fn elem<'a, E>(input: &'a str) -> IResult<&'a str, (&'a str, Cow<'a, str>), E>
where
    E: ParseError<&'a str>,
{
    let (input, name) = any_start_tag(input)?;
    let (input, value) = elem_value(input)?;
    let value = match value {
        Cow::Borrowed(v) => Cow::Borrowed(v.trim()),
        Cow::Owned(v) => Cow::Owned(String::from(v.trim())),
    };

    match value.len() {
        0 => map(
            terminated(
                recognize(many0_count(tuple((
                    not(peek(tuple((multispace0, end_tag(name))))),
                    take(1u8),
                )))),
                tuple((multispace0, end_tag(name))),
            ),
            |value: &str| (name, Cow::Borrowed(value)),
        )(input),
        _ => Ok((input, (name, value))),
    }
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use nom::{
        bytes::complete::tag,
        error::{Error, ErrorKind},
    };
    use test_case::test_case;

    use crate::parsers::test_utils::{assert_parser, Expected};

    #[test_case(""                   , Ok("")                   , ""   ; "eof"                   )]
    #[test_case("<"                  , Ok("")                   , "<"  ; "first is angle bracket")]
    #[test_case("&"                  , Ok("")                   , "&"  ; "first is ampersand"    )]
    #[test_case("abc123ABC!@# \t\r\n", Ok("abc123ABC!@# \t\r\n"), ""   ; "no special chars"      )]
    #[test_case("a1A!<x"             , Ok("a1A!")               , "<x" ; "angle bracket"         )]
    #[test_case("a1A!&x"             , Ok("a1A!")               , "&x" ; "ampersand in middle"   )]
    fn normal_chars(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::normal_chars, input, expected, remaining);
    }

    #[test_case(""                   , Err(ErrorKind::IsNot)    , ""   ; "eof"                   )]
    #[test_case("<"                  , Err(ErrorKind::IsNot)    , "<"  ; "first is angle bracket")]
    #[test_case("&"                  , Err(ErrorKind::IsNot)    , "&"  ; "first is ampersand"    )]
    #[test_case("abc123ABC!@# \t\r\n", Ok("abc123ABC!@# \t\r\n"), ""   ; "no special chars"      )]
    #[test_case("a1A!<x"             , Ok("a1A!")               , "<x" ; "angle bracket"         )]
    #[test_case("a1A!&x"             , Ok("a1A!")               , "&x" ; "ampersand in middle"   )]
    fn normal_chars1(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::normal_chars1, input, expected, remaining);
    }

    #[test_case(">"          , Err(ErrorKind::IsA), ">"       ; "empty"        )]
    #[test_case("lower>"     , Err(ErrorKind::IsA), "lower>"  ; "lowercase"    )]
    #[test_case("2UPPER>"    , Err(ErrorKind::IsA), "2UPPER>" ; "number prefix")]
    #[test_case("A>"         , Ok("A")            , ">"       ; "single char"  )]
    #[test_case("UPPER>"     , Ok("UPPER")        , ">"       ; "multi char"   )]
    #[test_case("UPPER2>"    , Ok("UPPER2")       , ">"       ; "with number"  )]
    #[test_case("WITH SPACE>", Ok("WITH")         , " SPACE>" ; "whitespace"   )]
    fn tag_name(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::tag_name, input, expected, remaining);
    }

    #[test_case("ASDF"   , Err(ErrorKind::Tag), "ASDF"   ; "no delimiters"      )]
    #[test_case("ASDF>"  , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter" )]
    #[test_case("<ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"   )]
    #[test_case("<>"     , Err(ErrorKind::IsA), ">"      ; "empty name"         )]
    #[test_case("</ASDF>", Err(ErrorKind::IsA), "/ASDF>" ; "end-style delimiter")]
    #[test_case("<AS DF>", Err(ErrorKind::Tag), " DF>"   ; "whitespace"         )]
    #[test_case("<ASDF>" , Ok("ASDF")         , ""       ; "valid tag"          )]
    fn start_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::any_start_tag, input, expected, remaining);
    }

    #[test_case("ASDF"   , Err(ErrorKind::Tag), "ASDF"   ; "no delimiters"      )]
    #[test_case("ASDF>"  , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter" )]
    #[test_case("<ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"   )]
    #[test_case("<>"     , Err(ErrorKind::Tag), ">"      ; "empty name"         )]
    #[test_case("</ASDF>", Err(ErrorKind::Tag), "/ASDF>" ; "end-style delimiter")]
    #[test_case("<OTHER>", Err(ErrorKind::Tag), "OTHER>" ; "name mismatch"      )]
    #[test_case("<ASDF>" , Ok("ASDF")         , ""       ; "valid tag"          )]
    fn named_start_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::start_tag("ASDF"), input, expected, remaining);
    }

    #[test_case("ASDF"   , Err(ErrorKind::Tag), "ASDF"    ; "no delimiters"        )]
    #[test_case("ASDF>"   , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter"   )]
    #[test_case("</ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"     )]
    #[test_case("</>"     , Err(ErrorKind::IsA), ">"      ; "empty name"           )]
    #[test_case("<ASDF>"  , Err(ErrorKind::Tag), "<ASDF>" ; "start-style delimiter")]
    #[test_case("</AS DF>", Err(ErrorKind::Tag), " DF>"   ; "whitespace"           )]
    #[test_case("</ASDF>" , Ok("ASDF")         , ""       ; "valid tag"            )]
    fn end_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::any_end_tag, input, expected, remaining);
    }

    #[test_case("ASDF"    , Err(ErrorKind::Tag), "ASDF"   ; "no delimiters"        )]
    #[test_case("ASDF>"   , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter"   )]
    #[test_case("</ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"     )]
    #[test_case("</>"     , Err(ErrorKind::Tag), ">"      ; "empty name"           )]
    #[test_case("<ASDF>"  , Err(ErrorKind::Tag), "<ASDF>" ; "start-style delimiter")]
    #[test_case("</OTHER>", Err(ErrorKind::Tag), "OTHER>" ; "name mismatch"        )]
    #[test_case("</ASDF>" , Ok("ASDF")         , ""       ; "valid tag"            )]
    fn named_end_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::end_tag("ASDF"), input, expected, remaining);
    }

    #[test_case(""                , Ok(Cow::from(""))    , ""   ; "eof"                        )]
    #[test_case("<"               , Ok(Cow::from(""))    , "<"  ; "left angle bracket"         )]
    #[test_case("&"               , Ok(Cow::from("&"))   , ""   ; "ampersand"                  )]
    #[test_case("&lt;"            , Ok(Cow::from("<"))   , ""   ; "escaped left angle bracket" )]
    #[test_case("&gt;"            , Ok(Cow::from(">"))   , ""   ; "escaped right angle bracket")]
    #[test_case("&amp;"           , Ok(Cow::from("&"))   , ""   ; "escaped ampersand"          )]
    #[test_case("&nbsp;"          , Ok(Cow::from(" "))   , ""   ; "escaped space"              )]
    #[test_case("a1A!&lt;b2B@&gt;", Ok(Cow::from("a1A!<b2B@>")), "" ; "mixed escapes"          )]
    #[test_case("a&lt;<x"         , Ok(Cow::from("a<"))  , "<x" ; "escaped then left angle"    )]
    #[test_case("&lt;a<x"         , Ok(Cow::from("<a"))  , "<x" ; "normal then left angle"     )]
    #[test_case("a&lt;&x"         , Ok(Cow::from("a<&x")), ""   ; "escaped then ampersand"     )]
    #[test_case("&lt;a&x"         , Ok(Cow::from("<a&x")), ""   ; "normal then ampersand"      )]
    #[test_case("&lt;&gt;&nbsp;"  , Ok(Cow::from("<> ")) , ""   ; "repeated escapes"           )]
    #[test_case("&&&&"            , Ok(Cow::from("&&&&")), ""   ; "repeated ampersands"        )]
    fn elem_value<'a>(input: &'a str, expected: Expected<Cow<'a, str>>, remaining: &'a str) {
        assert_parser(super::elem_value, input, expected, remaining);
    }

    #[test_case("value"             , Err(ErrorKind::Tag),  "value"    ; "no start tag"       )]
    #[test_case("<TAG>value"        , Ok(("TAG", "value")), ""         ; "eof after value"    )]
    #[test_case("<TAG> \r\n\tvaluex", Ok(("TAG", "value")), "x"        ; "leading whitespace" )]
    #[test_case("<TAG>value \r\n\tx", Ok(("TAG", "value")), " \r\n\tx" ; "trailing whitespace")]
    fn any_value_elem<'a>(
        input: &'a str,
        expected: Expected<(&'a str, &'a str)>,
        remaining: &'a str,
    ) {
        assert_parser(
            super::any_value_elem(tag("value")),
            input,
            expected,
            remaining,
        );
    }

    #[test_case("value"             , Err(ErrorKind::Tag), "value"    ; "no start tag"       )]
    #[test_case("<NO>value"         , Err(ErrorKind::Tag), "NO>value" ; "name mismatch"      )]
    #[test_case("<TAG>value"        , Ok("value")        , ""         ; "eof after value"    )]
    #[test_case("<TAG> \r\n\tvaluex", Ok("value")        , "x"        ; "leading whitespace" )]
    #[test_case("<TAG>value \r\n\tx", Ok("value")        , " \r\n\tx" ; "trailing whitespace")]
    fn value_elem<'a>(input: &'a str, expected: Expected<&'a str>, remaining: &'a str) {
        assert_parser(
            super::value_elem("TAG", tag("value")),
            input,
            expected,
            remaining,
        );
    }

    #[test_case("value</TAG>"    , Err(ErrorKind::Tag), "value</TAG>"      ; "no start tag"   )]
    #[test_case("<TAG>value"     , Err(ErrorKind::Tag), ""                 ; "no end tag"     )]
    #[test_case("<TAG>value</NO>", Err(ErrorKind::Tag), "NO>"              ; "wrong end tag"  )]
    #[test_case("<TAG>value</TAG>"        , Ok(("TAG", "value")), ""   ; "eof after end tag"  )]
    #[test_case("<TAG> \r\n\tvalue</TAG>x", Ok(("TAG", "value")), "x"  ; "leading whitespace" )]
    #[test_case("<TAG>value \r\n\t</TAG>x", Ok(("TAG", "value")), "x"  ; "trailing whitespace")]
    fn any_group_elem<'a>(
        input: &'a str,
        expected: Expected<(&'a str, &'a str)>,
        remaining: &'a str,
    ) {
        assert_parser(
            super::any_group_elem(tag("value")),
            input,
            expected,
            remaining,
        );
    }

    #[test_case("value</TAG>"    , Err(ErrorKind::Tag),  "value</TAG>"   ; "no start tag"       )]
    #[test_case("<TAG>value"     , Err(ErrorKind::Tag),  ""              ; "no end tag"         )]
    #[test_case("<NO>value</NO>" , Err(ErrorKind::Tag),  "NO>value</NO>" ; "name mismatch"      )]
    #[test_case("<TAG>value</NO>", Err(ErrorKind::Tag),  "NO>"           ; "wrong end tag"      )]
    #[test_case("<TAG>value</TAG>"        , Ok("value"), ""              ; "eof after end tag"  )]
    #[test_case("<TAG> \r\n\tvalue</TAG>x", Ok("value"), "x"             ; "leading whitespace" )]
    #[test_case("<TAG>value \r\n\t</TAG>x", Ok("value"), "x"             ; "trailing whitespace")]
    fn group_elem<'a>(input: &'a str, expected: Expected<&'a str>, remaining: &'a str) {
        assert_parser(
            super::group_elem("TAG", tag("value")),
            input,
            expected,
            remaining,
        );
    }

    #[test_case("<TAG>value"        , Ok(("TAG", Cow::from("value")))  , "" ; "noend tag with eof")]
    #[test_case(
        "<TAG> value\r\n",
        Ok(("TAG", Cow::from("value"))),
        "" ;
        "noend tag with whitespace"
    )]
    #[test_case("<TAG><F></F></TAG>", Ok(("TAG", Cow::from("<F></F>"))), "" ; "tag with inner tag")]
    #[test_case(
        "<TAG>\r\n  <F></F>\r\n</TAG>",
        Ok(("TAG", Cow::from("<F></F>"))),
        "" ;
        "tag with inner tag and whitespace"
    )]
    fn elem<'a>(input: &'a str, expected: Expected<(&'a str, Cow<'a, str>)>, remaining: &'a str) {
        assert_parser(super::elem, input, expected, remaining);
    }
}
