use thiserror::Error;

#[derive(Clone, Debug, Error, PartialEq)]
pub enum OfxParseWarning {
    #[error("unrecognized OFXHEADER value `{0}`")]
    UnrecognizedOfxHeaderVersion(u32),
    #[error("unrecognized DATA value `{0}`")]
    UnrecognizedContentType(String),
    #[error("unrecognized VERSION value `{0}`")]
    UnrecognizedVersion(u32),
    #[error("unrecognized SECURITY value `{0}`")]
    UnrecognizedSecurityType(String),
    #[error("unrecognized CHARSET value `{0}`")]
    UnrecognizedCharset(String),
}

/// Augments a value with a list of `OfxParseWarning` values, indicating recoverable warnings which
/// occurred during parsing.
#[derive(Clone, Debug, PartialEq)]
pub struct Warn<T>
where
    T: Clone + std::fmt::Debug + PartialEq,
{
    pub value: T,
    pub warnings: Vec<OfxParseWarning>,
}

impl<T> From<T> for Warn<T>
where
    T: Clone + std::fmt::Debug + PartialEq,
{
    fn from(value: T) -> Self {
        Warn {
            value,
            warnings: Vec::new(),
        }
    }
}
