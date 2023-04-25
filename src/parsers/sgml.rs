//! Parsers for SGML-based (v1.x) OFX documents.

pub mod ofx_header;

use ofx_header::OfxHeader;

/// An OFX document.
pub struct OfxDocument {
    /// The header section of the document.
    pub header: OfxHeader,
}
