//! Module to handle custom serde `Serializer`
//!
//! This module serves as a patch for the quick_xml serde support.

mod var;

use quick_xml::DeError;
use serde::ser::Serialize;

struct ByteWriter<W>(W)
where
    W: std::io::Write;

impl<W: std::io::Write> std::fmt::Write for ByteWriter<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write(s.as_bytes()).map_err(|_| std::fmt::Error)?;
        std::fmt::Result::Ok(())
    }
}

impl<W: std::io::Write> std::io::Write for ByteWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

// trait WriteExt for ByteWriterVec {
//     fn write_bytes(&mut self, s: &[u8]) -> std::fmt::Result;
// }

// impl WriteExt for ByteWriterVec {
//     fn write_bytes(&mut self, s: &[u8]) -> std::fmt::Result {
//         std::fmt::Result::Ok(())
//     }
// }

pub struct Serializer<'w, 'r, W: std::fmt::Write> {
    pub ser: quick_xml::se::Serializer<'w, 'r, W>,
}

impl<'w, 'r, W: std::fmt::Write> Serializer<'w, 'r, W> {
    pub fn new(writer: &'w mut W) -> Self {
        Self {
            ser: quick_xml::se::Serializer::new(writer),
        }
    }
}

/// Implements serialization method by forwarding it to the serializer created by
/// the helper method [`Serializer::ser`].
macro_rules! forward {
    ($name:ident($ty:ty)) => {
        fn $name(self, value: $ty) -> Result<Self::Ok, Self::Error> {
            self.ser.$name(value)
        }
    };
}

impl<'w, 'r, W: std::fmt::Write> serde::ser::Serializer for Serializer<'w, 'r, W> {
    type Ok = ();
    type Error = DeError;

    type SerializeSeq =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeSeq;
    type SerializeTuple =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeTuple;
    type SerializeTupleStruct =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeTupleStruct;
    type SerializeTupleVariant =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeTupleVariant;
    type SerializeMap =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeMap;
    type SerializeStruct =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeStruct;
    type SerializeStructVariant =
        <quick_xml::se::Serializer<'w, 'r, W> as serde::ser::Serializer>::SerializeStructVariant;

    forward!(serialize_bool(bool));

    forward!(serialize_i8(i8));
    forward!(serialize_i16(i16));
    forward!(serialize_i32(i32));
    forward!(serialize_i64(i64));

    forward!(serialize_u8(u8));
    forward!(serialize_u16(u16));
    forward!(serialize_u32(u32));
    forward!(serialize_u64(u64));

    forward!(serialize_i128(i128));
    forward!(serialize_u128(u128));

    forward!(serialize_f32(f32));
    forward!(serialize_f64(f64));

    forward!(serialize_char(char));
    forward!(serialize_str(&str));

    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, DeError> {
        self.ser.serialize_bytes(value)
    }

    fn serialize_none(self) -> Result<Self::Ok, DeError> {
        self.ser.serialize_none()
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, DeError> {
        self.ser.serialize_some(value)
    }

    fn serialize_unit(self) -> Result<Self::Ok, DeError> {
        self.ser.serialize_unit()
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, DeError> {
        self.ser.serialize_unit_struct(name)
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, DeError> {
        self.ser
            .serialize_unit_variant(name, variant_index, variant)
    }

    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, DeError> {
        self.ser.serialize_newtype_struct(name, value)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, DeError> {
        self.ser
            .serialize_newtype_variant(name, variant_index, variant, value)
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, DeError> {
        self.ser.serialize_seq(len)
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, DeError> {
        self.ser.serialize_tuple(len)
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, DeError> {
        self.ser.serialize_tuple_struct(name, len)
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, DeError> {
        self.ser
            .serialize_tuple_variant(name, variant_index, variant, len)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, DeError> {
        self.ser.serialize_map(len)
    }

    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, DeError> {
        self.ser.serialize_struct(name, len)
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, DeError> {
        self.ser
            .serialize_struct_variant(name, variant_index, variant, len)
    }
}

/// Serialize struct into a `Write`r
pub fn to_writer<W: std::io::Write, S: Serialize>(writer: W, value: &S) -> Result<(), DeError> {
    let mut b = ByteWriter(writer);
    let serializer = Serializer::new(&mut b);
    value.serialize(serializer)
}

/// Serialize struct into a `String`
pub fn to_string<S: Serialize>(value: &S) -> Result<String, DeError> {
    Ok(String::from_utf8(to_bytes(value)?).map_err(|e| quick_xml::Error::from(e.utf8_error()))?)
}

/// Serialize struct into a `Vec<u8>`
pub fn to_bytes<S: Serialize>(value: &S) -> Result<Vec<u8>, DeError> {
    let mut bytes = ByteWriter(Vec::new());
    to_writer(&mut bytes, value)?;
    Ok(bytes.0)
}
