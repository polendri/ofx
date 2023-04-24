use nom::{
    branch::permutation,
    bytes::complete::tag,
    character::complete::{line_ending, u32},
    combinator::value,
    sequence::{delimited, tuple},
    IResult,
};

use crate::error::{OfxParseWarning, Warn};

#[derive(Debug, PartialEq)]
pub struct OfxHeader {
    pub header_version: u32,
    pub data: String,
    pub version: u32,
}

fn header_version(input: &str) -> IResult<&str, Warn<u32>> {
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

fn version(input: &str) -> IResult<&str, Warn<u32>> {
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

fn data(input: &str) -> IResult<&str, &str> {
    delimited(tag("DATA:"), tag("OFXSGML"), line_ending)(input)
}

pub fn ofx_header(input: &str) -> IResult<&str, Warn<OfxHeader>> {
    let (
        input,
        (
            Warn {
                value: header_version,
                warnings: header_version_warnings,
            },
            (
                data,
                Warn {
                    value: version,
                    warnings: version_warnings,
                },
            ),
            _,
        ),
    ) = tuple((header_version, permutation((data, version)), line_ending))(input)?;

    Ok((
        input,
        Warn {
            value: OfxHeader {
                header_version,
                data: String::from(data),
                version,
            },
            warnings: [header_version_warnings, version_warnings].concat(),
        },
    ))
}

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_version__unrecognized_value__ok_with_warn() {
        let result = header_version("OFXHEADER:999\r\n");

        assert_eq!(
            result,
            Ok((
                "",
                Warn {
                    value: 999,
                    warnings: vec![OfxParseWarning::UnrecognizedOfxHeaderVersion(999)]
                }
            ))
        );
    }

    #[test]
    fn header_version__valid_elem__ok() {
        let result = header_version("OFXHEADER:100\r\n");

        assert_eq!(result, Ok(("", Warn::from(100))));
    }

    #[test]
    fn data__valid_elem__ok() {
        let result = data("DATA:OFXSGML\r\n");

        assert_eq!(result, Ok(("", "OFXSGML")));
    }

    #[test]
    fn version__unrecognized_value__ok_with_warn() {
        let result = version("VERSION:101\r\n");

        assert_eq!(
            result,
            Ok((
                "",
                Warn {
                    value: 101,
                    warnings: vec![OfxParseWarning::UnrecognizedVersion(101)]
                }
            ))
        );
    }

    #[test]
    fn version__valid_elem__ok() {
        let result = version("VERSION:160\r\n");

        assert_eq!(result, Ok(("", Warn::from(160))));
    }

    #[test]
    fn ofx_header__header_with_warnings__propagates_warnings() {
        let result = ofx_header("OFXHEADER:999\r\nDATA:OFXSGML\r\nVERSION:101\r\n\r\n");

        let expected_value = OfxHeader {
            header_version: 999,
            data: String::from("OFXSGML"),
            version: 101,
        };
        let expected_warnings = vec![
            OfxParseWarning::UnrecognizedOfxHeaderVersion(999),
            OfxParseWarning::UnrecognizedVersion(101),
        ];
        assert_eq!(
            result,
            Ok((
                "",
                Warn {
                    value: expected_value,
                    warnings: expected_warnings
                }
            ))
        );
    }

    #[test]
    fn ofx_header__valid_header__ok() {
        let result = ofx_header("OFXHEADER:100\r\nDATA:OFXSGML\r\nVERSION:102\r\n\r\n");

        let expected = OfxHeader {
            header_version: 100,
            data: String::from("OFXSGML"),
            version: 102,
        };
        assert_eq!(result, Ok(("", Warn::from(expected))));
    }
}
