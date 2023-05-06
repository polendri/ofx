use crate::ofx::header::OfxHeader;

/// An OFX response document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxResponse<'a> {
    /// The header section of the document.
    pub header: OfxHeader<'a>,
    /// TODO
    pub ofx: &'a str,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SignonMessageSetChild {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxChild {
    SignonMessageSet(SignonMessageSetChild),
    Unknown { name: String, value: String },
}
