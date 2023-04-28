use nom::{
    branch::{alt, permutation},
    bytes::complete::tag,
    character::complete::{line_ending, not_line_ending, u32},
    combinator::{map, value},
    error::ParseError,
    sequence::{delimited, tuple},
    IResult,
};

use crate::error::{OfxParseWarning, Warn};
use crate::models::ofx_header::*;

/// Parses the `OFXHEADER` header element.
fn header_version_elem<'a, E>(input: &'a str) -> IResult<&'a str, Warn<u32>, E>
where
    E: ParseError<&'a str>,
{
    let (input, version) = delimited(tag("OFXHEADER:"), u32, line_ending)(input)?;
    Ok((
        input,
        Warn {
            value: version,
            warnings: if version == 100 {
                Vec::new()
            } else {
                vec![OfxParseWarning::UnrecognizedOfxHeaderVersion(version)]
            },
        },
    ))
}

/// Parses the `DATA` header element.
fn data_elem<'a, E>(input: &'a str) -> IResult<&'a str, Warn<OfxContentType>, E>
where
    E: ParseError<&'a str>,
{
    delimited(
        tag("DATA:"),
        alt((
            value(Warn::from(OfxContentType::OfxSgml), tag("OFXSGML")),
            map(not_line_ending, |v: &str| Warn {
                value: OfxContentType::Unknown(String::from(v)),
                warnings: vec![OfxParseWarning::UnrecognizedContentType(String::from(v))],
            }),
        )),
        line_ending,
    )(input)
}

/// Parses the `VERSION` header element.
fn version_elem<'a, E>(input: &'a str) -> IResult<&'a str, Warn<u32>, E>
where
    E: ParseError<&'a str>,
{
    let expected_versions = [102, 151, 160];
    let (input, version) = delimited(tag("VERSION:"), u32, line_ending)(input)?;
    Ok((
        input,
        Warn {
            value: version,
            warnings: if expected_versions.contains(&version) {
                Vec::new()
            } else {
                vec![OfxParseWarning::UnrecognizedVersion(version)]
            },
        },
    ))
}

/// Parses the `SECURITY` header element.
fn security_elem<'a, E>(input: &'a str) -> IResult<&'a str, Warn<OfxSecurity>, E>
where
    E: ParseError<&'a str>,
{
    delimited(
        tag("SECURITY:"),
        alt((
            value(Warn::from(OfxSecurity::None), tag("NONE")),
            value(Warn::from(OfxSecurity::Type1), tag("TYPE1")),
            map(not_line_ending, |v: &str| Warn {
                value: OfxSecurity::Unknown(String::from(v)),
                warnings: vec![OfxParseWarning::UnrecognizedSecurityType(String::from(v))],
            }),
        )),
        line_ending,
    )(input)
}

/// Parses the `ENCODING` header element.
fn encoding_elem<'a, E>(input: &'a str) -> IResult<&'a str, OfxEncoding, E>
where
    E: ParseError<&'a str>,
{
    delimited(
        tag("ENCODING:"),
        alt((
            value(OfxEncoding::UsAscii, tag("USASCII")),
            value(OfxEncoding::Utf8, tag("UTF-8")),
        )),
        line_ending,
    )(input)
}

/// Parses the `CHARSET` header element.
fn charset_elem<'a, E>(input: &'a str) -> IResult<&'a str, Warn<OfxCharset>, E>
where
    E: ParseError<&'a str>,
{
    delimited(
        tag("CHARSET:"),
        alt((
            value(Warn::from(OfxCharset::Latin1), tag("ISO-8859-1")),
            value(Warn::from(OfxCharset::WindowsLatin1), tag("1252")),
            value(Warn::from(OfxCharset::None), tag("NONE")),
            map(not_line_ending, |v: &str| Warn {
                value: OfxCharset::Unknown(String::from(v)),
                warnings: vec![OfxParseWarning::UnrecognizedCharset(String::from(v))],
            }),
        )),
        line_ending,
    )(input)
}

/// Parses the `COMPRESSION` header element.
fn compression_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("COMPRESSION:"), not_line_ending, line_ending)(input)
}

/// Parses the `OLDFILEUID` header element.
fn old_file_uid_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("OLDFILEUID:"), not_line_ending, line_ending)(input)
}

/// Parses the `NEWFILEUID` header element.
fn new_file_uid_elem<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    delimited(tag("NEWFILEUID:"), not_line_ending, line_ending)(input)
}

/// Parses the header of an OFX document.
pub fn ofx_header<'a, E>(input: &'a str) -> IResult<&'a str, Warn<OfxHeader>, E>
where
    E: ParseError<&'a str>,
{
    let (
        input,
        (
            Warn {
                value: header_version,
                warnings: header_version_warnings,
            },
            (
                Warn {
                    value: data,
                    warnings: data_warnings,
                },
                Warn {
                    value: version,
                    warnings: version_warnings,
                },
                Warn {
                    value: security,
                    warnings: security_warnings,
                },
                encoding,
                Warn {
                    value: charset,
                    warnings: charset_warnings,
                },
                compression,
                old_file_uid,
                new_file_uid,
            ),
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
        Warn {
            value: OfxHeader {
                header_version,
                data,
                version,
                security,
                encoding,
                charset,
                compression: String::from(compression),
                old_file_uid: String::from(old_file_uid),
                new_file_uid: String::from(new_file_uid),
            },
            warnings: [
                header_version_warnings,
                data_warnings,
                version_warnings,
                security_warnings,
                charset_warnings,
            ]
            .concat(),
        },
    ))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;
    use test_case::test_case;

    use crate::error::{OfxParseWarning, Warn};
    use crate::parsers::test_utils::{assert_parser, Expected};

    use super::*;

    #[test_case("ASDF:100\r\n"      , Err(ErrorKind::Tag)  , "ASDF:100\r\n" ; "invalid tag"  )]
    #[test_case("OFXHEADER:\r\n"    , Err(ErrorKind::Digit), "\r\n"         ; "empty value"  )]
    #[test_case("OFXHEADER:ASDF\r\n", Err(ErrorKind::Digit), "ASDF\r\n"     ; "invalid value")]
    #[test_case(
        "OFXHEADER:999\r\n",
        Ok(Warn {
            value: 999,
            warnings: vec![OfxParseWarning::UnrecognizedOfxHeaderVersion(999)],
        }),
        "" ;
        "unknown value"
    )]
    #[test_case("OFXHEADER:100\r\n" , Ok(Warn::from(100))  , ""             ; "known value"  )]
    fn header_version_elem(input: &str, expected: Expected<Warn<u32>>, remaining: &str) {
        assert_parser(super::header_version_elem, input, expected, remaining);
    }

    #[test_case("ASDF:OFXSGML\r\n", Err(ErrorKind::Tag), "ASDF:OFXSGML\r\n" ; "invalid tag"  )]
    #[test_case(
        "DATA:ofxsgml\r\n",
        Ok(Warn {
            value: OfxContentType::Unknown(String::from("ofxsgml")),
            warnings: vec![OfxParseWarning::UnrecognizedContentType(String::from("ofxsgml"))],
        }),
        "" ;
        "unknown value"
    )]
    #[test_case("DATA:OFXSGML\r\n" , Ok(Warn::from(OfxContentType::OfxSgml)), "" ; "known value"  )]
    fn data_elem(input: &str, expected: Expected<Warn<OfxContentType>>, remaining: &str) {
        assert_parser(super::data_elem, input, expected, remaining);
    }

    #[test_case("ASDF:100\r\n"    , Err(ErrorKind::Tag)  , "ASDF:100\r\n" ; "invalid tag"  )]
    #[test_case("VERSION:\r\n"    , Err(ErrorKind::Digit), "\r\n"         ; "empty value"  )]
    #[test_case("VERSION:ASDF\r\n", Err(ErrorKind::Digit), "ASDF\r\n"     ; "invalid value")]
    #[test_case(
        "VERSION:101\r\n",
        Ok(Warn {
            value: 101,
            warnings: vec![OfxParseWarning::UnrecognizedVersion(101)],
        }),
        "" ;
        "unknown value"
    )]
    #[test_case("VERSION:102\r\n" , Ok(Warn::from(102))  , ""             ; "known value 102")]
    #[test_case("VERSION:151\r\n" , Ok(Warn::from(151))  , ""             ; "known value 151")]
    #[test_case("VERSION:160\r\n" , Ok(Warn::from(160))  , ""             ; "known value 160")]
    fn version_elem(input: &str, expected: Expected<Warn<u32>>, remaining: &str) {
        assert_parser(super::version_elem, input, expected, remaining);
    }

    #[test_case("ASDF:NONE\r\n", Err(ErrorKind::Tag), "ASDF:NONE\r\n" ; "invalid tag")]
    #[test_case(
        "SECURITY:\r\n",
        Ok(Warn {
            value: OfxSecurity::Unknown(String::from("")),
            warnings: vec![OfxParseWarning::UnrecognizedSecurityType(String::from(""))],
        }),
        "" ;
        "empty"
    )]
    #[test_case(
        "SECURITY:type1\r\n",
        Ok(Warn {
            value: OfxSecurity::Unknown(String::from("type1")),
            warnings: vec![OfxParseWarning::UnrecognizedSecurityType(String::from("type1"))],
        }),
        "" ;
        "unknown value"
    )]
    #[test_case("SECURITY:NONE\r\n" , Ok(Warn::from(OfxSecurity::None)),  "" ; "known value none" )]
    #[test_case("SECURITY:TYPE1\r\n", Ok(Warn::from(OfxSecurity::Type1)), "" ; "known value type1")]
    fn security_elem(input: &str, expected: Expected<Warn<OfxSecurity>>, remaining: &str) {
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
        Ok(Warn {
            value: OfxHeader {
                header_version: 999,
                data: OfxContentType::Unknown(String::from("ASDF1")),
                version: 101,
                security: OfxSecurity::Unknown(String::from("ASDF2")),
                encoding: OfxEncoding::UsAscii,
                charset: OfxCharset::Unknown(String::from("ASDF3")),
                compression: String::new(),
                old_file_uid: String::from("ASDF4"),
                new_file_uid: String::from("ASDF5"),
            },
            warnings: vec![
                OfxParseWarning::UnrecognizedOfxHeaderVersion(999),
                OfxParseWarning::UnrecognizedContentType(String::from("ASDF1")),
                OfxParseWarning::UnrecognizedVersion(101),
                OfxParseWarning::UnrecognizedSecurityType(String::from("ASDF2")),
                OfxParseWarning::UnrecognizedCharset(String::from("ASDF3")),
            ],
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
        Ok(Warn::from(OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: String::new(),
            old_file_uid: String::from("OLD"),
            new_file_uid: String::from("NEW"),
        })),
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
        Ok(Warn::from(OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: String::new(),
            old_file_uid: String::from("OLD"),
            new_file_uid: String::from("NEW"),
        })),
        "x" ;
        "valid out-of-order header"
    )]
    fn ofx_header(input: &str, expected: Expected<Warn<OfxHeader>>, remaining: &str) {
        assert_parser(super::ofx_header, input, expected, remaining);
    }
}
