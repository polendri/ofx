mod error;
pub mod parsers;

use nom::IResult;

pub use error::OfxParseWarning;
use error::Warn;
use parsers::sgml::ofx_header::{ofx_header, OfxHeader};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxResponse {
    pub header: OfxHeader,
}

pub fn ofx_response(input: &str) -> IResult<&str, Warn<OfxResponse>> {
    let (
        input,
        Warn {
            value: header,
            warnings: header_warnings,
        },
    ) = ofx_header(input)?;
    Ok((
        input,
        Warn {
            value: OfxResponse { header },
            warnings: [header_warnings].concat(),
        },
    ))
}
