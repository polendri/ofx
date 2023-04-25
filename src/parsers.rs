//! Parsers for OFX documents.

pub mod sgml;

#[allow(non_snake_case)]
#[cfg(test)]
pub mod test_utils {
    use nom::{error::Error, Err, IResult};

    pub fn assert_parser_ok<'a, P, O>(parser: P, input: &'a str, expected: O)
    where
        P: FnOnce(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Debug + PartialEq,
    {
        let result = parser(input);

        assert_eq!(result, Ok(("", expected)));
    }

    pub fn assert_parser_err<'a, P, O>(parser: P, input: &'a str, expected: Error<&'a str>)
    where
        P: FnOnce(&'a str) -> IResult<&'a str, O>,
        O: std::fmt::Debug + PartialEq,
    {
        let result = parser(input);

        assert_eq!(result, Err(Err::Error(expected)));
    }
}
