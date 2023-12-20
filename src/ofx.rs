use serde::Deserialize;
use time::OffsetDateTime;

use self::header::OfxHeader;
pub use self::header::*;

pub mod header;

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum Severity {
    Info,
    Warn,
    Error,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "STATUS", rename_all = "UPPERCASE")]
pub struct StatusV1<'a> {
    pub code: i32,
    pub severity: Severity,
    pub message: &'a str,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "SONRS", rename_all = "UPPERCASE")]
pub struct SignonResponse<'a> {
    #[serde(borrow)]
    pub status: StatusV1<'a>,
    // TODO: Copy this desrializer impl https://docs.rs/time/latest/src/time/serde/iso8601.rs.html
    //       in order to implement the OFX 1.6 3.2.8.2 page-70 datetime spec
    pub dtserver: OffsetDateTime,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "SIGNONMSGSRSV1", rename_all = "UPPERCASE")]
pub struct SignonMessageSetV1<'a> {
    #[serde(borrow)]
    pub sonrs: Option<SignonResponse<'a>>,
}

/// An OFX response document.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(rename = "OFX", rename_all = "UPPERCASE")]
pub struct OfxRoot<'a> {
    // TODO: support v2
    #[serde(borrow)]
    pub signonmsgsrsv1: Option<SignonMessageSetV1<'a>>,
}

/// An OFX document.
#[derive(Clone, Debug, PartialEq)]
pub struct Ofx<'a> {
    /// The header section of the document.
    pub header: OfxHeader<'a>,
    /// The root of the document.
    pub ofx: OfxRoot<'a>,
}
