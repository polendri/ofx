//! Parsers for OFX SGML elements.

use std::borrow::Cow;

use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, take, take_until},
    character::complete::multispace0,
    combinator::{eof, map, peek, recognize, value, verify},
    error::ParseError,
    multi::{many0, many_till},
    sequence::{delimited, tuple},
    IResult, Parser,
};

/// Consumes whitespace before the provided parser.
pub(crate) fn whitespace_preceded<'a, O, E, P>(
    mut p: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, _) = multispace0(input)?;
        p.parse(input)
    }
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
pub(crate) fn elem_value<'a, E>(input: &'a str) -> IResult<&'a str, Cow<'a, str>, E>
where
    E: ParseError<&'a str>,
{
    const CDATA_END: &str = "]]>";

    map(
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
        ))),
        |vs| match vs.len() {
            0 => Cow::Borrowed(""),
            1 => Cow::Borrowed(vs[0]),
            _ => Cow::Owned(vs.concat()),
        },
    )(input)
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use nom::error::ErrorKind;
    use test_case::test_case;

    use crate::parse::test_utils::{assert_parser, Expected};

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

    #[test_case(""                , Ok(Cow::from(""))              , ""   ; "eof"          )]
    #[test_case("&lt;"            , Ok(Cow::from("<"))           , ""   ; "escaped left angle bracket" )]
    #[test_case("&gt;"            , Ok(Cow::from(">"))           , ""   ; "escaped right angle bracket")]
    #[test_case("&amp;"           , Ok(Cow::from("&"))           , ""   ; "escaped ampersand"          )]
    #[test_case("&nbsp;"          , Ok(Cow::from(" "))           , ""   ; "escaped space"              )]
    #[test_case("<![CDATA[<>& []a1A!]]>", Ok(Cow::from("<>& []a1A!")), ""   ; "escaped cdata"          )]
    #[test_case("a1A!&lt;b2B@&gt;", Ok(Cow::from("a1A!<b2B@>")), "" ; "mixed escapes"      )]
    #[test_case("a&lt;<x"         , Ok(Cow::from("a<"))      , "<x" ; "escaped then left angle"    )]
    #[test_case("&lt;a<x"         , Ok(Cow::from("<a"))      , "<x" ; "normal then left angle"     )]
    #[test_case("a&lt;&x"         , Ok(Cow::from("a<&x")), ""   ; "escaped then ampersand"     )]
    #[test_case("&lt;a&x"         , Ok(Cow::from("<a&x"))    , ""   ; "normal then ampersand"      )]
    #[test_case("&lt;&gt;&nbsp;"  , Ok(Cow::from("<> ")) , ""   ; "repeated escapes"           )]
    #[test_case("&&&&"            , Ok(Cow::from("&&&&"))        , ""   ; "repeated ampersands"        )]
    fn elem_value<'a>(input: &'a str, expected: Expected<Cow<'a, str>>, remaining: &'a str) {
        assert_parser(super::elem_value, input, expected, remaining);
    }
}
