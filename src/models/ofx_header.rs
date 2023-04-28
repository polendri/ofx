#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxContentType {
    OfxSgml,
    Unknown(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxSecurity {
    None,
    Type1,
    Unknown(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OfxEncoding {
    UsAscii,
    Utf8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OfxCharset {
    Latin1,
    WindowsLatin1,
    None,
    Unknown(String),
}

/// The header segment of an OFX document.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OfxHeader {
    /// The version of the header portion of the document. `100` is the only valid value; other
    /// values are tolerated, yielding a warning.
    pub header_version: u32,
    /// The content type of the document. `OFXSGML` is the only valid value; other values are
    /// tolerated, yielding a warning.
    pub data: OfxContentType,
    /// The version of the content portion of the document. `102`, `151` and `160` are the only
    /// valid values; other values yield a warning.
    pub version: u32,
    /// The type of application-level security used for the `<OFX>` block. Valid values are `NONE`
    /// and `TYPE1`; other values are tolerated, yielding a warning.
    pub security: OfxSecurity,
    /// The text encoding used for character data.
    pub encoding: OfxEncoding,
    /// The character set used for character data. Valid values are `ISO-8859-1`, `1252` and `NONE`;
    /// other values are tolerated, yielding a warning.
    pub charset: OfxCharset,
    /// Unused.
    pub compression: String,
    /// Intended to be used in conjunction with `new_file_uid` for file-based error recovery.
    pub old_file_uid: String,
    /// Uniquely identifies a request file.
    pub new_file_uid: String,
}
