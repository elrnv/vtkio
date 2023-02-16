//! Module to handle custom serde `Serializer`
//!
//! This module serves as a patch for the quick_xml serde support implementing unit variant
//! serialization into strings.

mod var;

use quick_xml::{se::Write, DeError};
use serde::ser::Serialize;

//use var::{Map, Seq}; //, Struct};

/// Serialize struct into a `Write`r
pub fn to_writer<W: Write, S: Serialize>(writer: W, value: &S) -> Result<W, DeError> {
    let serializer = quick_xml::se::Serializer::new(writer);
    value.serialize(serializer)
}

/// Serialize struct into a `String`
pub fn to_string<S: Serialize>(value: &S) -> Result<String, DeError> {
    Ok(String::from_utf8(to_bytes(value)?).map_err(|e| quick_xml::Error::from(e.utf8_error()))?)
}

/// Serialize struct into a `Vec<u8>`
pub fn to_bytes<S: Serialize>(value: &S) -> Result<Vec<u8>, DeError> {
    let mut bytes = Vec::new();
    to_writer(&mut bytes, value)?;
    Ok(bytes)
}
