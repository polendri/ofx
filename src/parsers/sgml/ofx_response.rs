//! Parsers for SGML-based (v1.x) OFX documents.

use nom::branch::alt;
use nom::combinator::map;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::{character::complete::multispace0, IResult};

use crate::error::Warn;
use crate::models::ofx_header::OfxHeader;
use crate::models::ofx_response::*;
use crate::parsers::sgml::element::{elem, group_elem};
use crate::parsers::sgml::ofx_header::ofx_header;

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
        group_elem(
            "OFX",
            many0(alt((map(elem, |(name, value)| OfxChild::Unknown {
                name: String::from(name),
                value: value.into_owned(),
            }),))),
        ),
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
