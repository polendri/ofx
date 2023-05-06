use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_a, tag},
    character::complete::{line_ending, not_line_ending, u32},
    combinator::{map, recognize, value},
    error::ParseError,
    multi::many0_count,
    sequence::{terminated, tuple},
    IResult, Parser,
};

use crate::ofx::header::*;

/// Parses a header element name.
fn elem_name<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    recognize(tuple((
        is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
        many0_count(is_a("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")),
    )))(input)
}

/// Parses a header element value.
fn elem_value<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    not_line_ending(input)
}

/// Parses an unknown element.
fn any_elem<'a, E>(input: &'a str) -> IResult<&'a str, (&'a str, &'a str), E>
where
    E: ParseError<&'a str>,
{
    tuple((
        terminated(elem_name, tag(":")),
        terminated(elem_value, line_ending),
    ))(input)
}

/// Parses an element.
fn elem<'a, O, E, P>(name: &'a str, mut p: P) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseError<&'a str>,
    P: Parser<&'a str, O, E>,
{
    move |input: &str| {
        let (input, _) = terminated(tag(name), tag(":"))(input)?;
        let (input, value) = p.parse(input)?;
        let (input, _) = line_ending(input)?;
        Ok((input, value))
    }
}

/// Parses the `OFXHEADER` header element.
fn header_version_elem<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
    E: ParseError<&'a str>,
{
    elem("OFXHEADER", u32)(input)
}

/// Parses the `DATA` header element.
fn data_elem<'a, E>(input: &'a str) -> IResult<&'a str, OfxContentType, E>
where
    E: ParseError<&'a str>,
{
    elem(
        "DATA",
        alt((
            value(OfxContentType::OfxSgml, tag("OFXSGML")),
            map(not_line_ending, |v| {
                OfxContentType::Unknown(String::from(v))
            }),
        )),
    )(input)
}

/// Parses the `VERSION` header element.
fn version_elem<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
    E: ParseError<&'a str>,
{
    elem("VERSION", u32)(input)
}

/// Parses the `SECURITY` header element.
fn security_elem<'a, E>(input: &'a str) -> IResult<&'a str, OfxSecurity, E>
where
    E: ParseError<&'a str>,
{
    elem(
        "SECURITY",
        alt((
            value(OfxSecurity::None, tag("NONE")),
            value(OfxSecurity::Type1, tag("TYPE1")),
            map(not_line_ending, |v| OfxSecurity::Unknown(String::from(v))),
        )),
    )(input)
}

/// Parses the `ENCODING` header element.
fn encoding_elem<'a, E>(input: &'a str) -> IResult<&'a str, OfxEncoding, E>
where
    E: ParseError<&'a str>,
{
    elem(
        "ENCODING",
        alt((
            value(OfxEncoding::UsAscii, tag("USASCII")),
            value(OfxEncoding::Utf8, tag("UTF-8")),
        )),
    )(input)
}

/// Parses the `CHARSET` header element.
fn charset_elem<'a, E>(input: &'a str) -> IResult<&'a str, OfxCharset, E>
where
    E: ParseError<&'a str>,
{
    elem(
        "CHARSET",
        alt((
            value(OfxCharset::Latin1, tag("ISO-8859-1")),
            value(OfxCharset::WindowsLatin1, tag("1252")),
            value(OfxCharset::None, tag("NONE")),
            map(not_line_ending, |v| OfxCharset::Unknown(String::from(v))),
        )),
    )(input)
}

/// Parses the `COMPRESSION` header element.
fn compression_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    elem("COMPRESSION", not_line_ending)(input)
}

/// Parses the `OLDFILEUID` header element.
fn old_file_uid_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    elem("OLDFILEUID", not_line_ending)(input)
}

/// Parses the `NEWFILEUID` header element.
fn new_file_uid_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    elem("NEWFILEUID", not_line_ending)(input)
}

/// Parses the header of an OFX document.
pub(crate) fn ofx_header<'a, E>(input: &'a str) -> IResult<&'a str, OfxHeader<'a>, E>
where
    E: ParseError<&'a str>,
{
    let (
        input,
        (
            header_version,
            (data, version, security, encoding, charset, compression, old_file_uid, new_file_uid),
        ),
    ) = tuple((
        header_version_elem,
        permutation((
            data_elem,
            version_elem,
            security_elem,
            encoding_elem,
            charset_elem,
            compression_elem,
            old_file_uid_elem,
            new_file_uid_elem,
        )),
    ))(input)?;

    Ok((
        input,
        OfxHeader {
            header_version,
            data,
            version,
            security,
            encoding,
            charset,
            compression,
            old_file_uid,
            new_file_uid,
        },
    ))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;
    use test_case::test_case;

    use crate::parse::test_utils::{assert_parser, Expected};

    use super::*;

    #[test_case(""          , Err(ErrorKind::IsA), ""      ; "eof"           )]
    #[test_case(":"         , Err(ErrorKind::IsA), ":"     ; "empty"         )]
    #[test_case("1SDF:"     , Err(ErrorKind::IsA), "1SDF:" ; "leading digit" )]
    #[test_case("ASD1:"     , Ok("ASD1")         , ":"     ; "trailing digit")]
    #[test_case("OFXHEADER:", Ok("OFXHEADER")    , ":"     ; "known value"   )]
    #[test_case(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:",
        Ok("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),
        ":" ;
        "all allowed chars"
    )]
    fn elem_name(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::elem_name, input, expected, remaining);
    }

    #[test_case(""             , Err(ErrorKind::IsA), ""              ; "eof"          )]
    #[test_case(":VAL\r\nx"    , Err(ErrorKind::IsA), ":VAL\r\nx"     ; "empty name"   )]
    #[test_case("1AME:VAL\r\nx", Err(ErrorKind::IsA), "1AME:VAL\r\nx" ; "leading digit")]
    #[test_case("NAME:\r\nx"   , Ok(("NAME", ""))   , "x"             ; "empty value"  )]
    #[test_case("NAME:VAL\r\nx", Ok(("NAME", "VAL")), "x"             ; "text value"   )]
    #[test_case(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:VAL\r\nx",
        Ok(("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", "VAL")),
        "x" ;
        "all allowed name chars"
    )]
    fn any_elem(input: &str, expected: Expected<(&str, &str)>, remaining: &str) {
        assert_parser(super::any_elem, input, expected, remaining);
    }

    #[test_case(""             , Err(ErrorKind::Tag), ""              ; "eof"           )]
    #[test_case(":VAL\r\nx"    , Err(ErrorKind::Tag), ":VAL\r\nx"     ; "empty name"    )]
    #[test_case("NOPE:VAL\r\nx", Err(ErrorKind::Tag), "NOPE:VAL\r\nx" ; "name mismatch" )]
    #[test_case("NAME:\r\nx"   , Err(ErrorKind::Tag), "\r\nx"         ; "value mismatch")]
    #[test_case("NAME:VAL\r\nx", Ok("VAL")          , "x"             ; "matched value" )]
    fn elem(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::elem("NAME", tag("VAL")), input, expected, remaining);
    }

    #[test_case("ASDF:100\r\n"      , Err(ErrorKind::Tag)  , "ASDF:100\r\n" ; "invalid tag"  )]
    #[test_case("OFXHEADER:\r\n"    , Err(ErrorKind::Digit), "\r\n"         ; "empty value"  )]
    #[test_case("OFXHEADER:ASDF\r\n", Err(ErrorKind::Digit), "ASDF\r\n"     ; "invalid value")]
    #[test_case("OFXHEADER:100\r\n" , Ok(100)              , ""             ; "valid value"  )]
    fn header_version_elem(input: &str, expected: Expected<u32>, remaining: &str) {
        assert_parser(super::header_version_elem, input, expected, remaining);
    }

    #[test_case("ASDF:OFXSGML\r\n", Err(ErrorKind::Tag), "ASDF:OFXSGML\r\n" ; "invalid tag")]
    #[test_case(
        "DATA:ofxsgml\r\n",
        Ok(OfxContentType::Unknown(String::from("ofxsgml"))),
        "" ;
        "unknown value"
    )]
    #[test_case("DATA:OFXSGML\r\n", Ok(OfxContentType::OfxSgml), "" ; "known value")]
    fn data_elem(input: &str, expected: Expected<OfxContentType>, remaining: &str) {
        assert_parser(super::data_elem, input, expected, remaining);
    }

    #[test_case("ASDF:100\r\n"    , Err(ErrorKind::Tag)  , "ASDF:100\r\n" ; "invalid tag"  )]
    #[test_case("VERSION:\r\n"    , Err(ErrorKind::Digit), "\r\n"         ; "empty value"  )]
    #[test_case("VERSION:ASDF\r\n", Err(ErrorKind::Digit), "ASDF\r\n"     ; "invalid value")]
    #[test_case("VERSION:102\r\n" , Ok(102)              , ""             ; "valid value"  )]
    fn version_elem(input: &str, expected: Expected<u32>, remaining: &str) {
        assert_parser(super::version_elem, input, expected, remaining);
    }

    #[test_case("ASDF:NONE\r\n", Err(ErrorKind::Tag), "ASDF:NONE\r\n" ; "invalid tag")]
    #[test_case(
        "SECURITY:\r\n",
        Ok(OfxSecurity::Unknown(String::from(""))),
        "" ;
        "empty"
    )]
    #[test_case(
        "SECURITY:type1\r\n",
        Ok(OfxSecurity::Unknown(String::from("type1"))),
        "" ;
        "unknown value"
    )]
    #[test_case("SECURITY:NONE\r\n" , Ok(OfxSecurity::None),  "" ; "known value none" )]
    #[test_case("SECURITY:TYPE1\r\n", Ok(OfxSecurity::Type1), "" ; "known value type1")]
    fn security_elem(input: &str, expected: Expected<OfxSecurity>, remaining: &str) {
        assert_parser(super::security_elem, input, expected, remaining);
    }

    #[test_case("ASDF:100\r\n"        , Err(ErrorKind::Tag)     , "ASDF:100\r\n" ; "invalid tag"  )]
    #[test_case("ENCODING:\r\n"       , Err(ErrorKind::Tag)     , "\r\n"         ; "empty value"  )]
    #[test_case("ENCODING:usascii\r\n", Err(ErrorKind::Tag)     , "usascii\r\n"  ; "invalid value")]
    #[test_case("ENCODING:USASCII\r\n", Ok(OfxEncoding::UsAscii), ""       ; "known value usascii")]
    #[test_case("ENCODING:UTF-8\r\n"  , Ok(OfxEncoding::Utf8)   , ""       ; "known value utf-8"  )]
    fn encoding_elem(input: &str, expected: Expected<OfxEncoding>, remaining: &str) {
        assert_parser(super::encoding_elem, input, expected, remaining);
    }

    #[test_case("ASDF:100\r\n"        , Err(ErrorKind::Tag), "ASDF:100\r\n" ; "invalid tag"   )]
    #[test_case("COMPRESSION:\r\n"    , Ok("")             , ""             ; "empty value"   )]
    #[test_case("COMPRESSION:ASDF\r\n", Ok("ASDF")         , ""             ; "nonempty value")]
    fn compression_elem(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::compression_elem, input, expected, remaining);
    }

    #[test_case("ASDF:ASDF\r\n"      , Err(ErrorKind::Tag), "ASDF:ASDF\r\n" ; "invalid tag"    )]
    #[test_case("OLDFILEUID:\r\n"    , Ok("")             , ""              ; "empty value"    )]
    #[test_case("OLDFILEUID: A1#\r\n", Ok(" A1#")         , ""              ; "nonempty value" )]
    fn old_file_uid_elem(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::old_file_uid_elem, input, expected, remaining);
    }

    #[test_case("ASDF:ASDF\r\n"      , Err(ErrorKind::Tag), "ASDF:ASDF\r\n" ; "invalid tag"    )]
    #[test_case("NEWFILEUID:\r\n"    , Ok("")             , ""              ; "empty value"    )]
    #[test_case("NEWFILEUID: A1#\r\n", Ok(" A1#")         , ""              ; "nonempty value" )]
    fn new_file_uid_elem(input: &str, expected: Expected<&str>, remaining: &str) {
        assert_parser(super::new_file_uid_elem, input, expected, remaining);
    }

    #[test_case(
        "DATA:OFXSGML\r\n\
         OFXHEADER:100\r\n\
         VERSION:102\r\n\
         SECURITY:NONE\r\n\
         ENCODING:USASCII\r\n\
         CHARSET:1252\r\n\
         COMPRESSION:\r\n\
         OLDFILEUID:OLD\r\n\
         NEWFILEUID:NEW\r\n\r\n",
        Err(ErrorKind::Tag),
        "DATA:OFXSGML\r\n\
         OFXHEADER:100\r\n\
         VERSION:102\r\n\
         SECURITY:NONE\r\n\
         ENCODING:USASCII\r\n\
         CHARSET:1252\r\n\
         COMPRESSION:\r\n\
         OLDFILEUID:OLD\r\n\
         NEWFILEUID:NEW\r\n\r\n" ;
        "ofxheader elem not first"
    )]
    #[test_case(
        "OFXHEADER:999\r\n\
         DATA:ASDF1\r\n\
         VERSION:101\r\n\
         SECURITY:ASDF2\r\n\
         ENCODING:USASCII\r\n\
         CHARSET:ASDF3\r\n\
         COMPRESSION:\r\n\
         OLDFILEUID:ASDF4\r\n\
         NEWFILEUID:ASDF5\r\n",
        Ok(OfxHeader {
                header_version: 999,
                data: OfxContentType::Unknown(String::from("ASDF1")),
                version: 101,
                security: OfxSecurity::Unknown(String::from("ASDF2")),
                encoding: OfxEncoding::UsAscii,
                charset: OfxCharset::Unknown(String::from("ASDF3")),
                compression: "",
                old_file_uid: "ASDF4",
                new_file_uid: "ASDF5",

        }),
        "" ;
        "propagates warnings"
    )]
    #[test_case(
        "OFXHEADER:100\r\n\
         DATA:OFXSGML\r\n\
         VERSION:102\r\n\
         SECURITY:NONE\r\n\
         ENCODING:USASCII\r\n\
         CHARSET:1252\r\n\
         COMPRESSION:\r\n\
         OLDFILEUID:OLD\r\n\
         NEWFILEUID:NEW\r\nx",
        Ok(OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: "",
            old_file_uid: "OLD",
            new_file_uid: "NEW",
        }),
        "x" ;
        "valid ordered header"
    )]
    #[test_case(
        // Note: OFXHEADER must always come first
        "OFXHEADER:100\r\n\
         COMPRESSION:\r\n\
         VERSION:102\r\n\
         ENCODING:USASCII\r\n\
         OLDFILEUID:OLD\r\n\
         CHARSET:1252\r\n\
         DATA:OFXSGML\r\n\
         NEWFILEUID:NEW\r\n\
         SECURITY:NONE\r\nx",
        Ok(OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: "",
            old_file_uid: "OLD",
            new_file_uid: "NEW",
        }),
        "x" ;
        "valid out-of-order header"
    )]
    fn ofx_header(input: &str, expected: Expected<OfxHeader>, remaining: &str) {
        assert_parser(super::ofx_header, input, expected, remaining);
    }
}
