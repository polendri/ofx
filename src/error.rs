use std::fmt;

use serde::de;
use thiserror::Error as ThisError;

#[derive(Clone, Debug, PartialEq, ThisError)]
pub enum Error {
    #[error("escape sequences in enum variant names are not supported")]
    EscapesInEnumVariant,
    #[error("expected borrowed str is invalid due to escape sequences in the input")]
    InvalidBorrowedStr,
    #[error("sequence ended but more tuple elements were expected")]
    InvalidTupleLength,
    #[error("trailing input remaining")]
    TrailingInput,
    #[error("parse error")]
    ParseError(String),
    #[error("parser expected more data")]
    ParseIncomplete,
    #[error("unsupported data type")]
    UnsupportedDataType,
    #[error("unknown error")]
    Unknown(String),
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::Unknown(format!("{}", msg))
    }
}

/// Alias for a `Result` with the error type `ofx::Error`.
pub type Result<T> = core::result::Result<T, Error>;
