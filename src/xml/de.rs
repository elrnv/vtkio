//! Module to handle custom serde `Deserializer`
//!
//! This module serves as a patch for the quick_xml serde support.

use quick_xml::DeError;
use serde::de::Deserialize;
use std::fmt::Read;

struct ByteReaderVec(Vec<u8>);

impl Write for ByteWriterVec {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        std::fmt::Result::Ok(())
    }
}

impl WriteExt for ByteWriterVec {}
