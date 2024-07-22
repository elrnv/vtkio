//! Module to handle custom serde `Serializer`
//!
//! This module serves as a patch for the quick_xml serde support.

use quick_xml::DeError;
use serde::ser::Serialize;

/// Serialize struct into a `Write`r
pub fn to_writer<W: std::io::Write, S: Serialize>(writer: W, value: &S) -> Result<(), DeError> {
    let mut buf_writer = std::io::BufWriter::new(writer);
    let serializer = quick_xml::se::io::Serializer::new(&mut buf_writer);
    value.serialize(serializer)
}

/// Serialize struct into a `String`
pub fn to_string<S: Serialize>(value: &S) -> Result<String, DeError> {
    let mut s = String::new();
    let serializer = quick_xml::se::Serializer::new(&mut s);
    value.serialize(serializer)?;
    Ok(s)
}

/// Serialize struct into a `Vec<u8>`
pub fn to_bytes<S: Serialize>(value: &S) -> Result<Vec<u8>, DeError> {
    let mut bytes = Vec::new();
    to_writer(&mut bytes, value)?;
    Ok(bytes)
}
