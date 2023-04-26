//! Parsers for SGML-based (v1.x) OFX documents.

mod element;
mod ofx_header;

use nom::IResult;

pub use self::ofx_header::{ofx_header, OfxHeader};
use crate::error::Warn;

/// An OFX response document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxResponse {
    /// The header section of the document.
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
