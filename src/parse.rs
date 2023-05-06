//! Parsers for OFX documents.

use std::ops::Deref;

use nom::{
    error::{convert_error, VerboseError},
    Err, Parser,
};

use crate::error::{Error, Result};

pub mod sgml;

pub(crate) fn wrap_nom<'a, O, P>(mut p: P, input: &'a str) -> Result<(&'a str, O)>
where
    P: Parser<&'a str, O, VerboseError<&'a str>>,
{
    p.parse(input).map_err(|e| match e {
        Err::Incomplete(_) => Error::ParseIncomplete,
        Err::Error(e) | Err::Failure(e) => Error::ParseError(convert_error(input, e)),
    })
}

#[allow(non_snake_case)]
#[cfg(test)]
pub mod test_utils {
    use nom::{
        error::{Error, ErrorKind},
        Err, IResult,
    };

    pub type Expected<O> = Result<O, ErrorKind>;

    pub fn assert_parser_ok<'a, P, O>(parser: P, input: &'a str, expected: O, remaining: &'a str)
    where
        P: FnOnce(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Debug + PartialEq,
    {
        let result = parser(input);

        assert_eq!(result, Ok((remaining, expected)));
    }

    pub fn assert_parser_err<'a, P, O>(
        parser: P,
        input: &'a str,
        kind: ErrorKind,
        remaining: &'a str,
    ) where
        P: FnOnce(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Debug + PartialEq,
    {
        let result = parser(input);

        assert_eq!(result, Err(Err::Error(Error::new(remaining, kind))));
    }

    pub fn assert_parser<'a, P, O>(
        parser: P,
        input: &'a str,
        expected: Expected<O>,
        remaining: &'a str,
    ) where
        P: FnOnce(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Debug + PartialEq,
    {
        match expected {
            Ok(exp) => assert_parser_ok(parser, input, exp, remaining),
            Err(kind) => assert_parser_err(parser, input, kind, remaining),
        }
    }
}
