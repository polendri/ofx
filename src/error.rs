use thiserror::Error;

#[derive(Copy, Clone, Debug, Error, PartialEq)]
pub enum OfxParseWarning {
    #[error("unrecognized OFXHEADER value `{0}`")]
    UnrecognizedOfxHeaderVersion(u32),
    #[error("unrecognized VERSION value `{0}`")]
    UnrecognizedVersion(u32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Warn<T> {
    pub value: T,
    pub warnings: Vec<OfxParseWarning>,
}

impl<T> From<T> for Warn<T> {
    fn from(value: T) -> Self {
        Warn {
            value,
            warnings: Vec::new(),
        }
    }
}
