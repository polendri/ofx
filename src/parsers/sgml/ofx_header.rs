use nom::{
    branch::{alt, permutation},
    bytes::complete::tag,
    character::complete::{line_ending, not_line_ending, u32},
    combinator::{map, value},
    sequence::{delimited, tuple},
    IResult,
};

use crate::error::{OfxParseWarning, Warn};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxContentType {
    OfxSgml,
    Unknown(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxSecurity {
    None,
    Type1,
    Unknown(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OfxEncoding {
    UsAscii,
    Utf8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxCharset {
    Latin1,
    WindowsLatin1,
    None,
    Unknown(String),
}

/// The header segment of an OFX document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxHeader {
    /// The version of the header portion of the document. `100` is the only valid value; other
    /// values are tolerated, yielding a warning.
    pub header_version: u32,
    /// The content type of the document. `OFXSGML` is the only valid value; other values are
    /// tolerated, yielding a warning.
    pub data: OfxContentType,
    /// The version of the content portion of the document. `102`, `151` and `160` are the only
    /// valid values; other values yield a warning.
    pub version: u32,
    /// The type of application-level security used for the `<OFX>` block. Valid values are `NONE`
    /// and `TYPE1`; other values are tolerated, yielding a warning.
    pub security: OfxSecurity,
    /// The text encoding used for character data.
    pub encoding: OfxEncoding,
    /// The character set used for character data. Valid values are `ISO-8859-1`, `1252` and `NONE`;
    /// other values are tolerated, yielding a warning.
    pub charset: OfxCharset,
    /// Unused.
    pub compression: String,
    /// Intended to be used in conjunction with `new_file_uid` for file-based error recovery.
    pub old_file_uid: String,
    /// Uniquely identifies a request file.
    pub new_file_uid: String,
}

/// Parses the `OFXHEADER` header element.
fn header_version_elem(input: &str) -> IResult<&str, Warn<u32>> {
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
fn data_elem(input: &str) -> IResult<&str, Warn<OfxContentType>> {
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
fn version_elem(input: &str) -> IResult<&str, Warn<u32>> {
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
fn security_elem(input: &str) -> IResult<&str, Warn<OfxSecurity>> {
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
fn encoding_elem(input: &str) -> IResult<&str, OfxEncoding> {
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
fn charset_elem(input: &str) -> IResult<&str, Warn<OfxCharset>> {
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
fn compression_elem(input: &str) -> IResult<&str, &str> {
    delimited(tag("COMPRESSION:"), not_line_ending, line_ending)(input)
}

/// Parses the `OLDFILEUID` header element.
fn old_file_uid_elem(input: &str) -> IResult<&str, &str> {
    delimited(tag("OLDFILEUID:"), not_line_ending, line_ending)(input)
}

/// Parses the `NEWFILEUID` header element.
fn new_file_uid_elem(input: &str) -> IResult<&str, &str> {
    delimited(tag("NEWFILEUID:"), not_line_ending, line_ending)(input)
}

/// Parses the header of an OFX document.
pub fn ofx_header(input: &str) -> IResult<&str, Warn<OfxHeader>> {
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
            _,
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
        line_ending,
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
    use nom::error::{Error, ErrorKind};
    use test_case::test_case;

    use super::*;
    use crate::parsers::test_utils::{assert_parser_err, assert_parser_ok};

    #[test_case("ASDF:100\r\n", Error::new("ASDF:100\r\n", ErrorKind::Tag) ; "invalid tag")]
    #[test_case("OFXHEADER:\r\n", Error::new("\r\n", ErrorKind::Digit) ; "empty value")]
    #[test_case("OFXHEADER:ASDF\r\n", Error::new("ASDF\r\n", ErrorKind::Digit) ; "invalid value")]
    fn header_version__invalid_elem__err(input: &str, expected: Error<&str>) {
        assert_parser_err(header_version_elem, input, expected);
    }

    #[test_case(
        "OFXHEADER:999\r\n",
        Warn {
            value: 999,
            warnings: vec![OfxParseWarning::UnrecognizedOfxHeaderVersion(999)],
        } ;
        "unknown value"
    )]
    #[test_case("OFXHEADER:100\r\n", Warn::from(100) ; "known value")]
    fn header_version__valid_elem__ok(input: &str, expected: Warn<u32>) {
        assert_parser_ok(header_version_elem, input, expected);
    }

    #[test]
    fn data__invalid_elem__err() {
        assert_parser_err(
            data_elem,
            "ASDF:OFXSGML\r\n",
            Error::new("ASDF:OFXSGML\r\n", ErrorKind::Tag),
        );
    }

    #[test_case(
        "DATA:ASDF\r\n",
        Warn {
            value: OfxContentType::Unknown(String::from("ASDF")),
            warnings: vec![OfxParseWarning::UnrecognizedContentType(String::from("ASDF"))],
        } ;
        "unknown value"
    )]
    #[test_case("DATA:OFXSGML\r\n", Warn::from(OfxContentType::OfxSgml) ; "known value")]
    fn data__valid_elem__ok(input: &str, expected: Warn<OfxContentType>) {
        assert_parser_ok(data_elem, input, expected);
    }

    #[test_case("ASDF:100\r\n", Error::new("ASDF:100\r\n", ErrorKind::Tag) ; "invalid tag")]
    #[test_case("VERSION:\r\n", Error::new("\r\n", ErrorKind::Digit) ; "empty value")]
    #[test_case("VERSION:ASDF\r\n", Error::new("ASDF\r\n", ErrorKind::Digit) ; "invalid value")]
    fn version__invalid_elem__err(input: &str, expected: Error<&str>) {
        assert_parser_err(version_elem, input, expected);
    }

    #[test_case(
        "VERSION:101\r\n",
        Warn {
            value: 101,
            warnings: vec![OfxParseWarning::UnrecognizedVersion(101)],
        } ;
        "unknown value"
    )]
    #[test_case("VERSION:102\r\n", Warn::from(102) ; "known value 102")]
    #[test_case("VERSION:151\r\n", Warn::from(151) ; "known value 151")]
    #[test_case("VERSION:160\r\n", Warn::from(160) ; "known value 160")]
    fn version__valid_elem__ok(input: &str, expected: Warn<u32>) {
        assert_parser_ok(version_elem, input, expected);
    }

    #[test]
    fn security__invalid_elem__err() {
        assert_parser_err(
            security_elem,
            "ASDF:NONE\r\n",
            Error::new("ASDF:NONE\r\n", ErrorKind::Tag),
        );
    }

    #[test_case(
        "SECURITY:\r\n",
        Warn {
            value: OfxSecurity::Unknown(String::from("")),
            warnings: vec![OfxParseWarning::UnrecognizedSecurityType(String::from(""))],
        } ;
        "empty"
    )]
    #[test_case(
        "SECURITY:ASDF\r\n",
        Warn {
            value: OfxSecurity::Unknown(String::from("ASDF")),
            warnings: vec![OfxParseWarning::UnrecognizedSecurityType(String::from("ASDF"))],
        } ;
        "unknown"
    )]
    #[test_case("SECURITY:NONE\r\n", Warn::from(OfxSecurity::None) ; "none")]
    #[test_case("SECURITY:TYPE1\r\n", Warn::from(OfxSecurity::Type1) ; "type1")]
    fn security__valid_elem__ok(input: &str, expected: Warn<OfxSecurity>) {
        assert_parser_ok(security_elem, input, expected);
    }

    #[test_case("ENCODING:\r\n", Error::new("\r\n", ErrorKind::Tag))]
    #[test_case("ENCODING:ASDF\r\n", Error::new("ASDF\r\n", ErrorKind::Tag))]
    fn encoding__invalid_value__err(input: &str, expected: Error<&str>) {
        assert_parser_err(encoding_elem, input, expected);
    }

    #[test_case("ENCODING:USASCII\r\n", OfxEncoding::UsAscii ; "ascii")]
    #[test_case("ENCODING:UTF-8\r\n", OfxEncoding::Utf8 ; "utf-8")]
    fn encoding__valid_elem__ok(input: &str, expected: OfxEncoding) {
        assert_parser_ok(encoding_elem, input, expected);
    }

    #[test]
    fn compression__invalid_elem__err() {
        assert_parser_err(
            compression_elem,
            "ASDF:ASDF\r\n",
            Error::new("ASDF:ASDF\r\n", ErrorKind::Tag),
        );
    }

    #[test_case("COMPRESSION:\r\n", "" ; "empty")]
    #[test_case("COMPRESSION: A1#\"\r\n", " A1#\"" ; "non-empty")]
    fn compression__valid_elem__ok(input: &str, expected: &str) {
        assert_parser_ok(compression_elem, input, expected);
    }

    #[test]
    fn old_file_uid__invalid_elem__err() {
        assert_parser_err(
            old_file_uid_elem,
            "ASDF:ASDF\r\n",
            Error::new("ASDF:ASDF\r\n", ErrorKind::Tag),
        );
    }

    #[test_case("OLDFILEUID:\r\n", "" ; "empty")]
    #[test_case("OLDFILEUID: A1#\"\r\n", " A1#\"" ; "non-empty")]
    fn old_file_uid__valid_elem__ok(input: &str, expected: &str) {
        assert_parser_ok(old_file_uid_elem, input, expected);
    }

    #[test]
    fn new_file_uid__invalid_elem__err() {
        assert_parser_err(
            new_file_uid_elem,
            "ASDF:ASDF\r\n",
            Error::new("ASDF:ASDF\r\n", ErrorKind::Tag),
        );
    }

    #[test_case("NEWFILEUID:\r\n", "" ; "empty")]
    #[test_case("NEWFILEUID: A1#\"\r\n", " A1#\"" ; "non-empty")]
    fn new_file_uid__valid_elem__ok(input: &str, expected: &str) {
        assert_parser_ok(new_file_uid_elem, input, expected);
    }

    #[test]
    fn ofx_header__header_with_warnings__propagates_warnings() {
        let input = "OFXHEADER:999\r\n\
                     DATA:ASDF1\r\n\
                     VERSION:101\r\n\
                     SECURITY:ASDF2\r\n\
                     ENCODING:USASCII\r\n\
                     CHARSET:ASDF3\r\n\
                     COMPRESSION:\r\n\
                     OLDFILEUID:ASDF4\r\n\
                     NEWFILEUID:ASDF5\r\n\r\n";
        let expected_value = OfxHeader {
            header_version: 999,
            data: OfxContentType::Unknown(String::from("ASDF1")),
            version: 101,
            security: OfxSecurity::Unknown(String::from("ASDF2")),
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::Unknown(String::from("ASDF3")),
            compression: String::new(),
            old_file_uid: String::from("ASDF4"),
            new_file_uid: String::from("ASDF5"),
        };
        let expected_warnings = vec![
            OfxParseWarning::UnrecognizedOfxHeaderVersion(999),
            OfxParseWarning::UnrecognizedContentType(String::from("ASDF1")),
            OfxParseWarning::UnrecognizedVersion(101),
            OfxParseWarning::UnrecognizedSecurityType(String::from("ASDF2")),
            OfxParseWarning::UnrecognizedCharset(String::from("ASDF3")),
        ];

        assert_parser_ok(
            ofx_header,
            input,
            Warn {
                value: expected_value,
                warnings: expected_warnings,
            },
        );
    }

    #[test]
    fn ofx_header__valid_ordered_header__ok() {
        let input = "OFXHEADER:100\r\n\
                     DATA:OFXSGML\r\n\
                     VERSION:102\r\n\
                     SECURITY:NONE\r\n\
                     ENCODING:USASCII\r\n\
                     CHARSET:1252\r\n\
                     COMPRESSION:\r\n\
                     OLDFILEUID:OLD\r\n\
                     NEWFILEUID:NEW\r\n\r\n";
        let expected = OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: String::new(),
            old_file_uid: String::from("OLD"),
            new_file_uid: String::from("NEW"),
        };

        assert_parser_ok(ofx_header, input, Warn::from(expected));
    }

    #[test]
    fn ofx_header__valid_unordered_header__ok() {
        // Note: OFXHEADER must always be first
        let input = "OFXHEADER:100\r\n\
                     COMPRESSION:\r\n\
                     VERSION:102\r\n\
                     ENCODING:USASCII\r\n\
                     OLDFILEUID:OLD\r\n\
                     CHARSET:1252\r\n\
                     DATA:OFXSGML\r\n\
                     NEWFILEUID:NEW\r\n\
                     SECURITY:NONE\r\n\r\n";
        let expected = OfxHeader {
            header_version: 100,
            data: OfxContentType::OfxSgml,
            version: 102,
            security: OfxSecurity::None,
            encoding: OfxEncoding::UsAscii,
            charset: OfxCharset::WindowsLatin1,
            compression: String::new(),
            old_file_uid: String::from("OLD"),
            new_file_uid: String::from("NEW"),
        };

        assert_parser_ok(ofx_header, input, Warn::from(expected));
    }
}
