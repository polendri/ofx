pub mod header;
pub mod response;

use serde::Deserialize;

use self::header::OfxHeader;

/// An OFX document.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct Ofx<'a, T> {
    /// The header section of the document.
    #[serde(borrow)]
    pub header: OfxHeader<'a>,
    /// TODO
    pub ofx: T,
}
