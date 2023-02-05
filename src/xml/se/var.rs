//!
//! This module is a patch for the quick_xml serde support implementing serialization of struct
//! fields renamed as `$value`.
//!

// use quick_xml::{events::BytesBytes, se::Serializer, DeError, Error};
// use serde::ser::{self, Serialize};
// use std::io::Write;

// use crate::xml::se::Serializer;

///// An implementation of `SerializeMap` for serializing to XML.
//pub struct Map<'w, W>
//where
//W: 'w + Write,
//{
//parent: &'w mut Serializer<W>,
//}

//impl<'w, W> Map<'w, W>
//where
//W: 'w + Write,
//{
///// Create a new Map
//pub fn new(parent: &'w mut Serializer<W>) -> Map<'w, W> {
//Map { parent }
//}
//}

//impl<'w, W> ser::SerializeMap for Map<'w, W>
//where
//    W: 'w + Write,
//{
//    type Ok = ();
//    type Error = DeError;
//
//    fn serialize_key<T: ?Sized + Serialize>(&mut self, _: &T) -> Result<(), DeError> {
//        Err(DeError::Unsupported(
//            "impossible to serialize the key on its own, please use serialize_entry()".into(),
//        ))
//    }
//
//    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DeError> {
//        value.serialize(&mut *self.parent)
//    }
//
//    fn end(self) -> Result<Self::Ok, DeError> {
//        Ok(())
//    }
//
//    fn serialize_entry<K: ?Sized + Serialize, V: ?Sized + Serialize>(
//        &mut self,
//        key: &K,
//        value: &V,
//    ) -> Result<(), DeError> {
//        // TODO: Is it possible to ensure our key is never a composite type?
//        // Anything which isn't a "primitive" would lead to malformed XML here...
//        write!(self.parent.writer.inner(), "<").map_err(Error::from)?;
//        key.serialize(&mut *self.parent)?;
//        write!(self.parent.writer.inner(), ">").map_err(Error::from)?;
//
//        value.serialize(&mut *self.parent)?;
//
//        write!(self.parent.writer.inner(), "</").map_err(Error::from)?;
//        key.serialize(&mut *self.parent)?;
//        write!(self.parent.writer.inner(), ">").map_err(Error::from)?;
//        Ok(())
//    }
//}

///// An implementation of `SerializeStruct` for serializing to XML.
//pub struct Struct<'w, W>
//where
//    W: 'w + Write,
//{
//    parent: &'w mut Serializer<W>,
//    name: &'w str,
//    /// Attributes represented by key-value pairs.
//    pub(crate) attrs: Vec<(Vec<u8>, Vec<u8>)>,
//    children: Vec<u8>,
//    buffer: Vec<u8>,
//    key_buffer: Vec<u8>,
//}
//
//impl<'w, W> Struct<'w, W>
//where
//    W: 'w + Write,
//{
//    /// Create a new `Struct`
//    pub fn new(parent: &'w mut Serializer<W>, name: &'w str) -> Struct<'w, W> {
//        Struct {
//            parent,
//            name,
//            attrs: Vec::new(),
//            children: Vec::new(),
//            buffer: Vec::new(),
//            key_buffer: Vec::new(),
//        }
//    }
//}
//
//impl<'w, W> ser::SerializeStruct for Struct<'w, W>
//where
//    W: 'w + Write,
//{
//    type Ok = ();
//    type Error = DeError;
//
//    fn serialize_field<T: ?Sized + Serialize>(
//        &mut self,
//        key: &'static str,
//        value: &T,
//    ) -> Result<(), DeError> {
//        let mut serializer = Serializer::new(&mut self.buffer);
//        value.serialize(&mut serializer)?;
//
//        if !self.buffer.is_empty() {
//            if key == "$value" {
//                self.children.extend(&self.buffer);
//            } else {
//                if self.buffer[0] == b'<' {
//                    if let Some(b) = self
//                        .buffer
//                        .iter()
//                        .position(|&x| x == b' ' || x == b'>' || x == b'/')
//                    {
//                        // Found tag boundary. Replace value tag with key.
//                        write!(&mut self.children, "<{}", key).map_err(Error::from)?;
//                        let buf_slice = &self.buffer[b..];
//                        if &buf_slice[buf_slice.len() - 2..] != b"/>" {
//                            if let Some(e) = buf_slice.iter().rposition(|&x| x == b'/') {
//                                // Find the end tag, replace value tag with key.
//                                self.children.extend(&buf_slice[..=e]);
//                                write!(&mut self.children, "{}>", key).map_err(Error::from)?;
//                            } else {
//                                return Err(DeError::Custom(String::from("end tag not found")));
//                            }
//                        } else {
//                            // Found a unit tag that of "<key/>" form.
//                            self.children.extend(buf_slice);
//                        }
//                    } else {
//                        write!(&mut self.children, "<{}>", key).map_err(Error::from)?;
//                        self.children.extend(&self.buffer);
//                        write!(&mut self.children, "</{}>", key).map_err(Error::from)?;
//                    }
//                } else {
//                    self.attrs
//                        .push((key.as_bytes().to_vec(), self.buffer.clone()));
//                    // write!(&mut self.attrs, " {}=\"", key).map_err(Error::Io)?;
//                    // self.attrs.extend(&self.buffer);
//                    // write!(&mut self.attrs, "\"").map_err(Error::Io)?;
//                }
//            }
//
//            self.buffer.clear();
//        }
//
//        Ok(())
//    }
//
//    fn end(self) -> Result<Self::Ok, DeError> {
//        let element_writer = self
//            .parent
//            .writer
//            .create_element(&self.name)
//            .with_attributes(self.attrs.iter().map(|(x, y)| (x.as_slice(), y.as_slice())));
//        if self.children.is_empty() {
//            element_writer.write_empty()?;
//        } else {
//            element_writer.write_bytes_content(BytesBytes::new(&self.children))?;
//            // element_writer.write__content(|w| w.write())?;
//        }
//        // self.parent.writer.write("<".as_bytes())?;
//        // self.parent.writer.write(&self.name.as_bytes())?;
//        // self.parent.writer.write(&self.attrs)?;
//        // if self.children.is_empty() {
//        //     self.parent.writer.write("/>".as_bytes())?;
//        // } else {
//        // self.parent.writer.write(">".as_bytes())?;
//        // self.parent.writer.write(&self.children)?;
//        // self.parent
//        //     .writer
//        //     .write_event(Event::End(BytesEnd::borrowed(self.name.as_bytes())))?;
//        //}
//        Ok(())
//    }
//}

//impl<'w, W> ser::SerializeStructVariant for Struct<'w, W>
//where
//    W: 'w + Write,
//{
//    type Ok = ();
//    type Error = DeError;
//
//    fn serialize_field<T: ?Sized + Serialize>(
//        &mut self,
//        key: &'static str,
//        value: &T,
//    ) -> Result<(), DeError> {
//        ser::SerializeStruct::serialize_field(self, key, value)
//    }
//
//    fn end(self) -> Result<Self::Ok, DeError> {
//        ser::SerializeStruct::end(self)
//    }
//}
//
//impl<'w, W> ser::SerializeMap for Struct<'w, W>
//where
//    W: 'w + Write,
//{
//    type Ok = ();
//    type Error = DeError;
//
//    fn serialize_key<T: ?Sized + Serialize>(&mut self, _: &T) -> Result<(), DeError> {
//        Err(DeError::Unsupported(
//            "impossible to serialize the key on its own, please use serialize_entry()".into(),
//        ))
//    }
//
//    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), DeError> {
//        value.serialize(&mut *self.parent)
//    }
//
//    fn end(self) -> Result<Self::Ok, DeError> {
//        ser::SerializeStruct::end(self)
//    }
//
//    fn serialize_entry<K: ?Sized + Serialize, V: ?Sized + Serialize>(
//        &mut self,
//        key: &K,
//        value: &V,
//    ) -> Result<(), DeError> {
//        let mut key_serializer = Serializer::new(&mut self.key_buffer);
//        key.serialize(&mut key_serializer)?;
//        let key = String::from_utf8_lossy(self.key_buffer.as_slice());
//
//        let mut serializer = Serializer::new(&mut self.buffer);
//        value.serialize(&mut serializer)?;
//
//        if !self.buffer.is_empty() {
//            if key == "$value" {
//                self.children.extend(&self.buffer);
//            } else {
//                if self.buffer[0] == b'<' {
//                    if let Some(b) = self
//                        .buffer
//                        .iter()
//                        .position(|&x| x == b' ' || x == b'>' || x == b'/')
//                    {
//                        // Found tag boundary. Replace value tag with key.
//                        write!(&mut self.children, "<{}", key).map_err(Error::from)?;
//                        let buf_slice = &self.buffer[b..];
//                        if &buf_slice[buf_slice.len() - 2..] != b"/>" {
//                            if let Some(e) = buf_slice.iter().rposition(|&x| x == b'/') {
//                                // Find the end tag, replace value tag with key.
//                                self.children.extend(&buf_slice[..=e]);
//                                write!(&mut self.children, "{}>", key).map_err(Error::from)?;
//                            } else {
//                                return Err(DeError::Custom(String::from("end tag not found")));
//                            }
//                        } else {
//                            // Found a unit tag that of "<key/>" form.
//                            self.children.extend(buf_slice);
//                        }
//                    } else {
//                        write!(&mut self.children, "<{}>", key).map_err(Error::from)?;
//                        self.children.extend(&self.buffer);
//                        write!(&mut self.children, "</{}>", key).map_err(Error::from)?;
//                    }
//                } else {
//                    // self.attrs
//                    //     .push((key.as_bytes(), self.buffer.as_slice()).into());
//                    self.attrs
//                        .push((key.as_bytes().to_vec(), self.buffer.clone()));
//                    //write!(&mut self.attrs, " {}=\"", key).map_err(Error::from)?;
//                    //self.attrs.extend(&self.buffer);
//                    //write!(&mut self.attrs, "\"").map_err(Error::from)?;
//                }
//            }
//
//            self.buffer.clear();
//        }
//        Ok(())
//    }
//}

///// An implementation of `SerializeSeq' for serializing to XML.
//pub struct Seq<'w, W>
//where
//W: 'w + Write,
//{
//parent: &'w mut Serializer<W>,
//}

//impl<'w, W> Seq<'w, W>
//where
//W: 'w + Write,
//{
///// Create a new `Seq`.
//pub fn new(parent: &'w mut Serializer<W>) -> Seq<'w, W> {
//Seq { parent }
//}
//}

//impl<'w, W> ser::SerializeSeq for Seq<'w, W>
//where
//W: 'w + Write,
//{
//type Ok = ();
//type Error = DeError;

//fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
//where
//T: Serialize,
//{
//value.serialize(&mut *self.parent)?;
//Ok(())
//}

//fn end(self) -> Result<Self::Ok, Self::Error> {
//Ok(())
//}
//}

//impl<'w, W> ser::SerializeTuple for Seq<'w, W>
//where
//W: 'w + Write,
//{
//type Ok = ();
//type Error = DeError;

//fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<(), Self::Error>
//where
//T: Serialize,
//{
//value.serialize(&mut *self.parent)?;
//Ok(())
//}

//fn end(self) -> Result<Self::Ok, Self::Error> {
//Ok(())
//}
//}
