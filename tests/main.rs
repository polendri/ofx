use test_case::test_case;

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
            signon_message_set_v1: Some(SignonMessageSetV1 {
                signon_response: Some(SignonResponse {
                    status: Some(StatusV1 {
                        code: 0,
                        severity: Severity::Info,
                        message: "OK\n   ",
                    }),
                })
            }),
        },
    }) ;
    "signon response"
)]
fn test_from_str(input: &str, expected: Result<Ofx>) {
    assert_eq!(from_str(input), expected);
}
