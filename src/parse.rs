//! Parsers for OFX documents.

use nom::{error::Error as BriefError, Parser};

pub mod sgml;

pub(crate) fn use_nom_opt<'a, O, P>(mut p: P, input: &'a str) -> Option<(&'a str, O)>
where
    P: Parser<&'a str, O, BriefError<&'a str>>,
{
    p.parse(input).ok()
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
