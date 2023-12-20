use test_case::test_case;
use time::macros::datetime;

use ofx_parse::{from_str, ofx::header::*, ofx::*, Result};

const HEADER: OfxHeader = OfxHeader {
    header_version: 100,
    data: OfxContentType::OfxSgml,
    version: 102,
    security: OfxSecurity::Type1,
    encoding: OfxEncoding::UsAscii,
    charset: OfxCharset::WindowsLatin1,
    compression: "NONE",
    old_file_uid: "NONE",
    new_file_uid: "NONE",
};

#[test_case(
    include_str!("data/v102/empty.ofx"),
    Ok(Ofx {
        header: HEADER,
        ofx: OfxRoot::default(),
    }) ;
    "empty OFX element"
)]
#[test_case(
    include_str!("data/v102/signon_response.ofx"),
    Ok(Ofx {
        header: HEADER,
        ofx: OfxRoot {
            signonmsgsrsv1: Some(SignonMessageSetV1 {
                sonrs: Some(SignonResponse {
                    status: StatusV1 {
                        code: 0,
                        severity: Severity::Info,
                        message: "OK\n   ",
                    },
                    dtserver: datetime!(2022-07-17 16:41:44 -8),
                })
            }),
        },
    }) ;
    "signon response"
)]
fn test_from_str(input: &str, expected: Result<Ofx>) {
    assert_eq!(from_str(input), expected);
}
