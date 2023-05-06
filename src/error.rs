use std::fmt;

use nom::{
    error::{convert_error, VerboseError},
    Err,
};
use serde::de;
use thiserror::Error as ThisError;

#[derive(Clone, Debug, PartialEq, ThisError)]
pub enum Error {
    #[error("trailing input remaining")]
    TrailingInput,
    #[error("parse error")]
    ParseError(String),
    #[error("parser expected more data")]
    ParseIncomplete,
    #[error("unknown error")]
    Unknown(String),
}

// impl From<nom::Err<nom::error::VerboseError<&str>>> for Error {
//     fn from(e: nom::Err<nom::error::VerboseError<&str>>) -> Self {
//         match e {
//             Err::Incomplete(_) => Error::ParseIncomplete,
//             Err::Error(e) | Err::Failure(e) => {
//                 let foo = convert_error(e);
//                 Error::ParseError(String::from("TODO"))
//             }
//         }
//     }
// }

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
