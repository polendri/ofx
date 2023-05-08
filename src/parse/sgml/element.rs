//! Parsers for OFX SGML elements.

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take, take_until},
    character::complete::multispace0,
    combinator::{eof, map, not, opt, peek, recognize, value, verify},
    error::ParseError,
    multi::{many0, many0_count, many_till},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};

/// Consumes whitespace before and after the provided parser.
pub(crate) fn whitespace_delimited<'a, O, E, P>(
    mut p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, _) = multispace0(input)?;
        let (input, value) = p.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, value))
    }
}

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
    is_not("<>&")(input)
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
pub(crate) fn any_start_tag<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("<"), tag_name, tag(">"))(input)
}

/// Parses a named start tag of an element.
pub(crate) fn start_tag<'a, E>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("<"), tag(name), tag(">"))
}

/// Parses the end tag of an element.
pub(crate) fn any_end_tag<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("</"), tag_name, tag(">"))(input)
}

/// Parses a named end tag of an element.
pub(crate) fn end_tag<'a, E>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("</"), tag(name), tag(">"))
}

/// Parses the value of an element.
pub(crate) fn elem_value<'a, E>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E>
where
    E: ParseError<&'a str>,
{
    const CDATA_END: &str = "]]>";

    many0(alt((
        value("<", tag("&lt;")),
        value(">", tag("&gt;")),
        value("&", tag("&amp;")),
        value(" ", tag("&nbsp;")),
        delimited(tag("<![CDATA["), take_until(CDATA_END), tag(CDATA_END)),
        verify(
            recognize(many_till(
                take(1u8),
                peek(alt((
                    tag("<"),
                    tag("&lt;"),
                    tag("&gt;"),
                    tag("&amp;"),
                    tag("&nbsp;"),
                    delimited(tag("<![CDATA["), take_until(CDATA_END), tag(CDATA_END)),
                    eof,
                ))),
            )),
            |o: &str| !o.is_empty(),
        ),
    )))(input)
}

/// Parses an element that has no end tag and a text value within it.
///
/// Trims whitespace before running the provided parser for the contents.
pub(crate) fn any_value_elem<'a, O, E, P>(
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
pub(crate) fn value_elem<'a, O, E, P>(
    name: &'a str,
    p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    preceded(tuple((start_tag(name), multispace0)), p)
}

/// Parses an element that has an end tag and other elements within it.
///
/// Trims whitespace before/after running the provided parser for the contents.
pub(crate) fn any_group_elem<'a, O, E, P>(
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
pub(crate) fn group_elem<'a, O, E, P>(
    name: &'a str,
    p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
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
pub(crate) fn any_elem<'a, E>(input: &'a str) -> IResult<&'a str, (&'a str, Vec<&'a str>), E>
where
    E: ParseError<&'a str>,
{
    let (input, name) = any_start_tag(input)?;
    let (input, mut value) = elem_value(input)?;
    if !value.is_empty() {
        value[0] = value[0].trim_start();
        let value_len = value.len();
        value[value_len - 1] = value[value_len - 1].trim_end();
    }

    if !value.is_empty() && !value[0].is_empty() {
        Ok((input, (name, value)))
    } else {
        map(
            terminated(
                recognize(many0_count(tuple((
                    not(peek(tuple((multispace0, end_tag(name))))),
                    take(1u8),
                )))),
                tuple((multispace0, end_tag(name))),
            ),
            |value: &str| (name, vec![value]),
        )(input)
    }
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use nom::{bytes::complete::tag, error::ErrorKind};
    use test_case::test_case;

    use crate::parse::test_utils::{assert_parser, Expected};

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

    #[test_case(""                , Ok(vec![])              , ""   ; "eof"          )]
    #[test_case("&lt;"            , Ok(vec!["<"])           , ""   ; "escaped left angle bracket" )]
    #[test_case("&gt;"            , Ok(vec![">"])           , ""   ; "escaped right angle bracket")]
    #[test_case("&amp;"           , Ok(vec!["&"])           , ""   ; "escaped ampersand"          )]
    #[test_case("&nbsp;"          , Ok(vec![" "])           , ""   ; "escaped space"              )]
    #[test_case("<![CDATA[<>& []a1A!]]>", Ok(vec!["<>& []a1A!"]), ""   ; "escaped cdata"          )]
    #[test_case("a1A!&lt;b2B@&gt;", Ok(vec!["a1A!", "<", "b2B@", ">"]), "" ; "mixed escapes"      )]
    #[test_case("a&lt;<x"         , Ok(vec!["a", "<"])      , "<x" ; "escaped then left angle"    )]
    #[test_case("&lt;a<x"         , Ok(vec!["<", "a"])      , "<x" ; "normal then left angle"     )]
    #[test_case("a&lt;&x"         , Ok(vec!["a", "<", "&x"]), ""   ; "escaped then ampersand"     )]
    #[test_case("&lt;a&x"         , Ok(vec!["<", "a&x"])    , ""   ; "normal then ampersand"      )]
    #[test_case("&lt;&gt;&nbsp;"  , Ok(vec!["<", ">", " "]) , ""   ; "repeated escapes"           )]
    #[test_case("&&&&"            , Ok(vec!["&&&&"])        , ""   ; "repeated ampersands"        )]
    fn elem_value<'a>(input: &'a str, expected: Expected<Vec<&'a str>>, remaining: &'a str) {
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

    #[test_case("<TAG>value"        , Ok(("TAG", vec!["value"]))  , "" ; "noend tag with eof")]
    #[test_case(
        "<TAG> value\r\n",
        Ok(("TAG", vec!["value"])),
        "" ;
        "noend tag with whitespace"
    )]
    #[test_case("<TAG><F></F></TAG>", Ok(("TAG", vec!["<F></F>"])), "" ; "tag with inner tag")]
    #[test_case(
        "<TAG>\r\n  <F></F>\r\n</TAG>",
        Ok(("TAG", vec!["<F></F>"])),
        "" ;
        "tag with inner tag and whitespace"
    )]
    fn any_elem<'a>(
        input: &'a str,
        expected: Expected<(&'a str, Vec<&'a str>)>,
        remaining: &'a str,
    ) {
        assert_parser(super::any_elem, input, expected, remaining);
    }
}
