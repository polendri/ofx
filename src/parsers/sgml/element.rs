use std::borrow::Cow;

use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_a, is_not, tag, take, take_until, take_while, take_while1},
    character::complete::{anychar, line_ending, multispace1, not_line_ending, space0, u32},
    combinator::{consumed, eof, fail, map, not, opt, peek, recognize, value},
    error::{Error, ErrorKind},
    multi::{many0, many_till},
    sequence::{delimited, preceded, terminated, tuple},
    Err, IResult,
};

/// An OFX element.
pub enum Element {
    Ofx,
    Unknown { tag: String, value: String },
}

/// Parses text devoid of special characters.
fn normal_chars(input: &str) -> IResult<&str, &str> {
    map(opt(is_not("<&")), |v| v.unwrap_or_default())(input)
}

/// Parses text devoid of special characters.
fn normal_chars1(input: &str) -> IResult<&str, &str> {
    is_not("<&")(input)
}

/// Parses the name of a tag.
fn tag_name(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        many0(is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")),
    )))(input)
}

/// Parses the start tag of an element.
fn start_tag(input: &str) -> IResult<&str, &str> {
    delimited(tag("<"), tag_name, tag(">"))(input)
}

/// Parses a named start tag of an element.
fn named_start_tag<'a>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(tag("<"), tag(name), tag(">"))
}

/// Parses the end tag of an element.
fn end_tag(input: &str) -> IResult<&str, &str> {
    delimited(tag("</"), tag_name, tag(">"))(input)
}

/// Parses a named end tag of an element.
fn named_end_tag<'a>(name: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(tag("</"), tag(name), tag(">"))
}

/// Parses the value of an element.
fn elem_value<'a>(input: &'a str) -> IResult<&'a str, Cow<'a, str>> {
    // let (input, chunks) = alt((
    //     value(Vec::new(), eof),
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

/// Parses an element with an end tag into a (tag, value) pair.
pub fn elem_with_end(input: &str) -> IResult<&str, (&str, &str)> {
    terminated(tuple((start_tag, take_while(|c: char| c != '<'))), end_tag)(input)
}

/// Parses an element without an end tag into a (tag, value) pair.
pub fn elem_no_end(input: &str) -> IResult<&str, (&str, &str)> {
    terminated(tuple((start_tag, not_line_ending)), alt((line_ending, eof)))(input)
}

/// Parses an element into a (name, value) pair.
///
/// Since the type of element defines whether it has an end tag in OFX, this necessarily has to take
/// a guess. If the start tag is followed by any non-whitespace before the next line
/// ending, then it is assumed not to have an end tag.
pub fn elem<'a>(input: &'a str) -> IResult<&'a str, (&'a str, &'a str)> {
    let (input, name) = start_tag(input)?;
    let (remaining, value) = alt((
        map(
            consumed(preceded(
                tuple((space0, line_ending)),
                many_till(take(1u8), recognize(named_end_tag(name))),
            )),
            // consumed(tuple((
            //     space0,
            //     line_ending,
            //     many_till(take(1u8), recognize(named_end_tag(name))),
            // ))),
            // `many_till()` includes the "till" parser in the result, no clean way to avoid that
            // except to use `consumed()` to get both the parsed input and the end tag and slice
            // the length of the end tag off the end
            |(cs, (_, et)): (&str, (Vec<&str>, &str))| &cs[0..(cs.len() - et.len())],
        ),
        terminated(not_line_ending, line_ending),
    ))(input)?;
    Ok((remaining, (name, value)))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use nom::error::{Error, ErrorKind};
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
        assert_parser(super::start_tag, input, expected, remaining);
    }

    #[test_case("ASDF"   , Err(ErrorKind::Tag), "ASDF"   ; "no delimiters"      )]
    #[test_case("ASDF>"  , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter" )]
    #[test_case("<ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"   )]
    #[test_case("<>"     , Err(ErrorKind::Tag), ">"      ; "empty name"         )]
    #[test_case("</ASDF>", Err(ErrorKind::Tag), "/ASDF>" ; "end-style delimiter")]
    #[test_case("<OTHER>", Err(ErrorKind::Tag), "OTHER>" ; "name mismatch"      )]
    #[test_case("<ASDF>" , Ok("ASDF")         , ""       ; "valid tag"          )]
    fn named_start_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::named_start_tag("ASDF"), input, expected, remaining);
    }

    #[test_case("ASDF"   , Err(ErrorKind::Tag), "ASDF"    ; "no delimiters"        )]
    #[test_case("ASDF>"   , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter"   )]
    #[test_case("</ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"     )]
    #[test_case("</>"     , Err(ErrorKind::IsA), ">"      ; "empty name"           )]
    #[test_case("<ASDF>"  , Err(ErrorKind::Tag), "<ASDF>" ; "start-style delimiter")]
    #[test_case("</AS DF>", Err(ErrorKind::Tag), " DF>"   ; "whitespace"           )]
    #[test_case("</ASDF>" , Ok("ASDF")         , ""       ; "valid tag"            )]
    fn end_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::end_tag, input, expected, remaining);
    }

    #[test_case("ASDF"    , Err(ErrorKind::Tag), "ASDF"   ; "no delimiters"        )]
    #[test_case("ASDF>"   , Err(ErrorKind::Tag), "ASDF>"  ; "no start delimiter"   )]
    #[test_case("</ASDF"  , Err(ErrorKind::Tag), ""       ; "no end delimiter"     )]
    #[test_case("</>"     , Err(ErrorKind::Tag), ">"      ; "empty name"           )]
    #[test_case("<ASDF>"  , Err(ErrorKind::Tag), "<ASDF>" ; "start-style delimiter")]
    #[test_case("</OTHER>", Err(ErrorKind::Tag), "OTHER>" ; "name mismatch"        )]
    #[test_case("</ASDF>" , Ok("ASDF")         , ""       ; "valid tag"            )]
    fn named_end_tag(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::named_end_tag("ASDF"), input, expected, remaining);
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

    // #[test_case("<TAG>VALUE\r\n", Error::new("", ErrorKind::Tag) ; "no end tag")]
    // #[test_case("VALUE</TAG>", Error::new("VALUE</TAG>", ErrorKind::Tag) ; "no start tag")]
    // fn elem_with_end__invalid_elem__err(input: &str, expected: Error<&str>) {
    //     assert_parser_err(elem_with_end, input, expected);
    // }

    // #[test_case("<TAG></TAG>x", ("TAG", ""), "x" ; "empty value")]
    // #[test_case("<TAG>VALUE</TAG>x", ("TAG", "VALUE"), "x" ; "no whitespace")]
    // #[test_case("<TAG> VALUE </TAG>x", ("TAG", " VALUE "), "x" ; "whitespace")]
    // #[test_case("<TAG>VAL\r\nUE\r\n</TAG>x", ("TAG", "VAL\r\nUE\r\n"), "x" ; "line endings")]
    // fn elem_with_end__valid_elem__ok(input: &str, expected: (&str, &str), remaining: &str) {
    //     assert_parser_ok(elem_with_end, input, expected, remaining)
    // }

    // #[test]
    // fn elem_no_end__no_start_tag__err() {
    //     assert_parser_err(
    //         elem_no_end,
    //         "VALUE\r\n",
    //         Error::new("VALUE\r\n", ErrorKind::Tag),
    //     );
    // }

    // #[test_case("<TAG>", ("TAG", ""), "" ; "empty eof")]
    // #[test_case("<TAG>\r\nx", ("TAG", ""), "x" ; "empty newline")]
    // #[test_case("<TAG>VALUE</TAG>\r\nx", ("TAG", "VALUE</TAG>"), "x" ; "false end tag")]
    // #[test_case("<TAG> VALUE \r\nx", ("TAG", " VALUE "), "x" ; "whitespace")]
    // fn elem_no_end__valid_elem__ok(input: &str, expected: (&str, &str), remaining: &str) {
    //     assert_parser_ok(elem_no_end, input, expected, remaining)
    // }

    // #[test]
    // fn elem__no_start_tag__err() {
    //     assert_parser_err(elem, "VALUE\r\n", Error::new("VALUE\r\n", ErrorKind::Tag));
    // }

    // #[test_case("<TAG>VALUE", ("TAG", "VALUE"), "" ; "ended by eof")]
    // #[test_case("<TAG></TAG>", ("TAG", ""), "" ; "empty with end tag")]
    // #[test_case("<TAG> VALUE\r\nx", ("TAG", " VALUE"), "x" ; "ended by newline")]
    // #[test_case("<TAG>VALUE</TAG>\r\nx", ("TAG", "VALUE"), "\r\nx" ; "with end tag")]
    // #[test_case("<TAG>VALUE\r\n  <WITH>INNER\r\n</TAG>x", ("TAG", "VALUE\r\n  <WITH>INNER\r\n"), "x" ; "with inner tag")]
    // fn elem__valid_elem__ok(input: &str, expected: (&str, &str), remaining: &str) {
    //     assert_parser_ok(elem, input, expected, remaining)
    // }
}
