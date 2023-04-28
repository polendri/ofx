use crate::models::ofx_header::OfxHeader;

/// An OFX response document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxResponse {
    /// The header section of the document.
    pub header: OfxHeader,
    /// TODO
    pub ofx: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SignonMessageSetChild {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxChild {
    SignonMessageSet(SignonMessageSetChild),
    Unknown { name: String, value: String },
}
