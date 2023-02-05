//! Module to handle custom serde `Serializer`
//!
//! This module serves as a patch for the quick_xml serde support implementing unit variant
//! serialization into strings.

mod var;

use quick_xml::{
    events::{BytesBytes, BytesContent, BytesEnd, BytesStart, Event},
    se::Write,
    DeError, Writer,
};
use serde::ser::{self, Impossible, Serialize};

//use var::{Map, Seq}; //, Struct};

/// Serialize struct into a `Write`r
pub fn to_writer<W: Write, S: Serialize>(writer: W, value: &S) -> Result<W, DeError> {
    let mut serializer = quick_xml::se::Serializer::new(writer);
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

///// A Serializer
//pub struct Serializer<W: Write> {
//    pub(crate) writer: Writer<W>,
//}
//
//impl<W: Write> Serializer<W> {
//    /// Creates a new `Serializer`
//    pub fn new(writer: W) -> Self {
//        Serializer {
//            writer: Writer::new(writer),
//        }
//    }
//
//    fn write_primitive<P: std::fmt::Display>(
//        &mut self,
//        value: P,
//        escaped: bool,
//    ) -> Result<(), DeError> {
//        let value = value.to_string();
//        let event = if escaped {
//            BytesText::from_escaped(value)
//        } else {
//            BytesText::new(&value)
//        };
//        self.writer.write_event(Event::Text(event))?;
//        Ok(())
//    }
//}

//impl<'w, W: Write> ser::Serializer for &'w mut Serializer<W> {
//    type Ok = ();
//    type Error = DeError;
//
//    type SerializeSeq = Seq<'w, W>;
//    type SerializeTuple = Seq<'w, W>;
//    type SerializeTupleStruct = Impossible<Self::Ok, DeError>;
//    type SerializeTupleVariant = Impossible<Self::Ok, DeError>;
//    type SerializeMap = Map<'w, W>;
//    type SerializeStruct = Struct<'w, W>;
//    type SerializeStructVariant = Struct<'w, W>;
//
//    fn serialize_bool(self, v: bool) -> Result<Self::Ok, DeError> {
//        self.write_primitive(if v { "true" } else { "false" }, true)
//    }
//
//    fn serialize_i8(self, v: i8) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_i16(self, v: i16) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_i32(self, v: i32) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_i64(self, v: i64) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_u8(self, v: u8) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_u16(self, v: u16) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_u32(self, v: u32) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_u64(self, v: u64) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_f32(self, v: f32) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_f64(self, v: f64) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, true)
//    }
//
//    fn serialize_char(self, v: char) -> Result<Self::Ok, DeError> {
//        self.write_primitive(v, false)
//    }
//
//    fn serialize_str(self, value: &str) -> Result<Self::Ok, DeError> {
//        self.write_primitive(value, false)
//    }
//
//    fn serialize_bytes(self, value: &[u8]) -> Result<Self::Ok, DeError> {
//        let event = BytesBytes::new(value);
//        self.writer.write_event(Event::Binary(event))?;
//        Ok(())
//    }
//
//    fn serialize_none(self) -> Result<Self::Ok, DeError> {
//        Ok(())
//    }
//
//    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<Self::Ok, DeError> {
//        value.serialize(self)
//    }
//
//    fn serialize_unit(self) -> Result<Self::Ok, DeError> {
//        self.serialize_none()
//    }
//
//    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, DeError> {
//        self.writer
//            .write_event(Event::Empty(BytesStart::new(name)))?;
//        Ok(())
//    }
//
//    fn serialize_unit_variant(
//        self,
//        _name: &'static str,
//        _variant_index: u32,
//        variant: &'static str,
//    ) -> Result<Self::Ok, DeError> {
//        self.serialize_str(variant)
//    }
//
//    fn serialize_newtype_struct<T: ?Sized + Serialize>(
//        self,
//        name: &'static str,
//        value: &T,
//    ) -> Result<Self::Ok, DeError> {
//        self.writer
//            .write_event(Event::Start(BytesStart::new(name)))?;
//        value.serialize(&mut *self)?;
//        self.writer.write_event(Event::End(BytesEnd::new(name)))?;
//        Ok(())
//    }
//
//    fn serialize_newtype_variant<T: ?Sized + Serialize>(
//        self,
//        _name: &'static str,
//        _variant_index: u32,
//        variant: &'static str,
//        value: &T,
//    ) -> Result<Self::Ok, DeError> {
//        self.writer
//            .write_event(Event::Start(BytesStart::new(variant)))?;
//        value.serialize(&mut *self)?;
//        self.writer
//            .write_event(Event::End(BytesEnd::new(variant)))?;
//        Ok(())
//    }
//
//    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, DeError> {
//        Ok(Seq::new(self))
//    }
//
//    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, DeError> {
//        Ok(Seq::new(self))
//    }
//
//    fn serialize_tuple_struct(
//        self,
//        _name: &'static str,
//        _len: usize,
//    ) -> Result<Self::SerializeTupleStruct, DeError> {
//        Err(DeError::Unsupported("serialize_tuple_struct".into()))
//    }
//
//    fn serialize_tuple_variant(
//        self,
//        _name: &'static str,
//        _variant_index: u32,
//        _variant: &'static str,
//        _len: usize,
//    ) -> Result<Self::SerializeTupleVariant, DeError> {
//        Err(DeError::Unsupported("serialize_tuple_variant".into()))
//    }
//
//    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, DeError> {
//        Ok(Map::new(self))
//    }
//
//    fn serialize_struct(
//        self,
//        name: &'static str,
//        _len: usize,
//    ) -> Result<Self::SerializeStruct, DeError> {
//        Ok(Struct::new(self, name))
//    }
//
//    fn serialize_struct_variant(
//        self,
//        _name: &'static str,
//        _variant_index: u32,
//        variant: &'static str,
//        _len: usize,
//    ) -> Result<Self::SerializeStructVariant, DeError> {
//        Ok(Struct::new(self, variant))
//    }
//}

//#[cfg(test)]
//mod tests {
//    use super::*;
//    use serde::ser::{SerializeMap, SerializeStruct};
//    use serde::{Serialize, Serializer as SerSerializer};
//
//    #[test]
//    fn test_serialize_bool() {
//        let inputs = vec![(true, "true"), (false, "false")];
//
//        for (src, should_be) in inputs {
//            let mut buffer = Vec::new();
//
//            {
//                let mut ser = Serializer::new(&mut buffer);
//                ser.serialize_bool(src).unwrap();
//            }
//
//            let got = String::from_utf8(buffer).unwrap();
//            assert_eq!(got, should_be);
//        }
//    }
//
//    #[test]
//    fn test_serialize_struct_field() {
//        let mut buffer = Vec::new();
//
//        {
//            let mut ser = Serializer::new(&mut buffer);
//            let mut struct_ser = Struct::new(&mut ser, "baz");
//            struct_ser.serialize_field("foo", "bar").unwrap();
//            let (key, value) = &struct_ser.attrs[0];
//            let attrs =
//                quick_xml::events::attributes::Attribute::from((key.as_slice(), value.as_slice()))
//                    .unescape_value()
//                    .unwrap();
//            assert_eq!(attrs, "bar");
//            let _ = SerializeStruct::end(struct_ser).unwrap();
//        }
//
//        let got = String::from_utf8(buffer).unwrap();
//        assert_eq!(got, "<baz foo=\"bar\"/>");
//    }
//
//    #[test]
//    fn test_serialize_struct() {
//        #[derive(Serialize)]
//        struct Person {
//            name: String,
//            age: u32,
//        }
//
//        let bob = Person {
//            name: "Bob".to_string(),
//            age: 42,
//        };
//        let should_be = "<Person name=\"Bob\" age=\"42\"/>";
//        let mut buffer = Vec::new();
//
//        {
//            let mut ser = Serializer::new(&mut buffer);
//            bob.serialize(&mut ser).unwrap();
//        }
//
//        let got = String::from_utf8(buffer).unwrap();
//        assert_eq!(got, should_be);
//    }
//
//    #[test]
//    fn test_serialize_map_entries() {
//        let should_be = "<name>Bob</name><age>5</age>";
//        let mut buffer = Vec::new();
//
//        {
//            let mut ser = Serializer::new(&mut buffer);
//            let mut map = Map::new(&mut ser);
//            map.serialize_entry("name", "Bob").unwrap();
//            map.serialize_entry("age", "5").unwrap();
//        }
//
//        let got = String::from_utf8(buffer).unwrap();
//        assert_eq!(got, should_be);
//    }
//
//    #[test]
//    fn test_serialize_enum() {
//        #[derive(Serialize)]
//        #[allow(dead_code)]
//        enum Node {
//            Boolean(bool),
//            Number(f64),
//            String(String),
//        }
//
//        let mut buffer = Vec::new();
//        let should_be = "<Boolean>true</Boolean>";
//
//        {
//            let mut ser = Serializer::new(&mut buffer);
//            let node = Node::Boolean(true);
//            node.serialize(&mut ser).unwrap();
//        }
//
//        let got = String::from_utf8(buffer).unwrap();
//        assert_eq!(got, should_be);
//    }
//
//    #[test]
//    #[ignore]
//    fn serialize_a_list() {
//        let inputs = vec![1, 2, 3, 4];
//
//        let mut buffer = Vec::new();
//
//        {
//            let mut ser = Serializer::new(&mut buffer);
//            inputs.serialize(&mut ser).unwrap();
//        }
//
//        let got = String::from_utf8(buffer).unwrap();
//        println!("{}", got);
//        panic!();
//    }
//}
