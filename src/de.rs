use crate::{de::sgml::from_str as from_sgml_str, error::Result, ofx::Ofx};

pub(crate) mod sgml;

pub fn from_str(s: &str) -> Result<Ofx> {
    from_sgml_str(s)
}
