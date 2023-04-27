//! Parsers for SGML-based (v1.x) OFX documents.

mod element;
mod ofx_header;

use nom::multi::many0;
use nom::sequence::tuple;
use nom::{character::complete::multispace0, IResult};

use self::element::{elem, group_elem};
use self::ofx_header::ofx_header;
pub use self::ofx_header::OfxHeader;
use crate::error::Warn;

/// An OFX response document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxResponse {
    /// The header section of the document.
    pub header: OfxHeader,
    /// TODO
    pub ofx: String,
}

pub fn ofx_response(input: &str) -> IResult<&str, Warn<OfxResponse>> {
    let (
        input,
        (
            _,
            Warn {
                value: header,
                warnings: header_warnings,
            },
            _,
            ofx,
            _,
        ),
    ) = tuple((
        multispace0,
        ofx_header,
        multispace0,
        group_elem("OFX", many0(elem)),
        multispace0,
    ))(input)?;

    Ok((
        input,
        Warn {
            value: OfxResponse {
                header,
                ofx: String::from("TODO"),
            },
            warnings: [header_warnings].concat(),
        },
    ))
}
