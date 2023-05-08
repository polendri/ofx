use std::fmt;

use serde::de;
use thiserror::Error as ThisError;

#[derive(Clone, Debug, PartialEq, ThisError)]
pub enum Error {
    #[error("expected borrowed str is invalid due to escape sequences in the input")]
    InvalidBorrowedStr,
    #[error("only unit and pair tuples are supported")]
    InvalidTupleLength,
    #[error("trailing input remaining")]
    TrailingInput,
    #[error("parse error")]
    ParseError(String),
    #[error("parser expected more data")]
    ParseIncomplete,
    #[error("unknown error")]
    Unknown(String),
}

impl de::Error for Error {
    #[cold]
    fn custom<T: fmt::Display>(msg: T) -> Error {
        Error::Unknown(format!("{}", msg))
    }

    #[cold]
    fn invalid_type(unexp: de::Unexpected, exp: &dyn de::Expected) -> Self {
        if let de::Unexpected::Unit = unexp {
            Error::custom(format_args!("invalid type: null, expected {}", exp))
        } else {
            Error::custom(format_args!("invalid type: {}, expected {}", unexp, exp))
        }
    }
}

/// Alias for a `Result` with the error type `ofx::Error`.
pub type Result<T> = core::result::Result<T, Error>;
