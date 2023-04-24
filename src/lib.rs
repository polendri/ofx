mod error;
mod ofx_header;

use nom::IResult;

pub use error::OfxParseWarning;
use error::Warn;
use ofx_header::ofx_header;
pub use ofx_header::OfxHeader;

#[derive(Debug, PartialEq)]
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

#[allow(non_snake_case)]
#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn it_works() {
    //     let result = add(2, 2);
    //     assert_eq!(result, 4);
    // }
}
