use serde::Deserialize;

use self::header::OfxHeader;
pub use self::header::*;

pub mod header;

#[derive(Clone, Debug, Deserialize, PartialEq)]
pub enum Severity {
    #[serde(rename = "INFO")]
    Info,
    #[serde(rename = "WARN")]
    Warn,
    #[serde(rename = "ERROR")]
    Error,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "STATUS")]
pub struct StatusV1<'a> {
    #[serde(rename = "CODE")]
    pub code: i32,
    #[serde(rename = "SEVERITY")]
    pub severity: Severity,
    #[serde(rename = "MESSAGE")]
    pub message: &'a str,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "SONRS")]
pub struct SignonResponse<'a> {
    #[serde(borrow, rename = "STATUS")]
    pub status: Option<StatusV1<'a>>,
}

#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename = "SIGNONMSGSRSV1")]
pub struct SignonMessageSetV1<'a> {
    #[serde(borrow, rename = "SONRS")]
    pub signon_response: Option<SignonResponse<'a>>,
}

/// An OFX response document.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
#[serde(rename = "OFX")]
pub struct OfxRoot<'a> {
    // TODO: support v2
    #[serde(borrow, rename = "SIGNONMSGSRSV1")]
    pub signon_message_set_v1: Option<SignonMessageSetV1<'a>>,
}

/// An OFX document.
#[derive(Clone, Debug, PartialEq)]
pub struct Ofx<'a> {
    /// The header section of the document.
    pub header: OfxHeader<'a>,
    /// The root of the document.
    pub ofx: OfxRoot<'a>,
}
