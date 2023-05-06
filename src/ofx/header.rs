use serde::Deserialize;
use serde_enum_str::Deserialize_enum_str;

#[derive(Clone, Debug, Deserialize_enum_str, Eq, PartialEq)]
pub enum OfxContentType {
    #[serde(rename = "OFXSGML")]
    OfxSgml,
    #[serde(other)]
    Unknown(String),
}

#[derive(Clone, Debug, Deserialize_enum_str, Eq, PartialEq)]
pub enum OfxSecurity {
    #[serde(rename = "NONE")]
    None,
    #[serde(rename = "TYPE1")]
    Type1,
    #[serde(other)]
    Unknown(String),
}

#[derive(Clone, Debug, Deserialize_enum_str, Eq, PartialEq)]
pub enum OfxEncoding {
    #[serde(rename = "USASCII")]
    UsAscii,
    #[serde(rename = "UTF-8")]
    Utf8,
    #[serde(other)]
    Unknown(String),
}

#[derive(Clone, Debug, Deserialize_enum_str, Eq, PartialEq)]
pub enum OfxCharset {
    #[serde(rename = "ISO-8859-1")]
    Latin1,
    #[serde(rename = "1252")]
    WindowsLatin1,
    #[serde(rename = "NONE")]
    None,
    #[serde(other)]
    Unknown(String),
}

/// The header segment of an OFX document.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct OfxHeader<'a> {
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
    pub compression: &'a str,
    /// Intended to be used in conjunction with `new_file_uid` for file-based error recovery.
    pub old_file_uid: &'a str,
    /// Uniquely identifies a request file.
    pub new_file_uid: &'a str,
}
