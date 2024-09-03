//!
//! Internal APIs for dealing with XML VTK file types.
//!
//! See [VTK XML Format
//! Reference](https://kitware.github.io/vtk-examples/site/VTKFileFormats/#xml-file-formats) for
//! details on the xml format.
//!

pub mod se;

use quick_xml::de;
use std::convert::{TryFrom, TryInto};
use std::io::BufRead;
use std::path::Path;

use base64::engine::general_purpose::STANDARD as BASE64_STANDARD;
use base64::Engine as _;
use log;
use serde::{Deserialize, Serialize};

use crate::model;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    XML(quick_xml::Error),
    Base64Decode(base64::DecodeError),
    Validation(ValidationError),
    Model(model::Error),
    IO(std::io::Error),
    Deserialization(de::DeError),
    InvalidVersion,
    TypeExtensionMismatch,
    InvalidType,
    InvalidByteOrder,
    //MissingAttribute(AttribName),
    //InvalidAttributeValueFor(AttribName),
    UnexpectedElement(String),
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::XML(source) => write!(f, "XML error: {:?}", source),
            Error::Base64Decode(source) => write!(f, "Base64 decode error: {:?}", source),
            Error::Validation(source) => write!(f, "Validation error: {:?}", source),
            Error::Model(source) => write!(f, "Model processing error: {:?}", source),
            Error::IO(source) => write!(f, "I/O error: {:?}", source),
            Error::Deserialization(source) => write!(f, "Deserialization error: {:?}", source),
            Error::InvalidVersion => write!(f, "VTK version must be in \"major.minor\" format"),
            Error::InvalidByteOrder => write!(
                f,
                "Byte order must be one of \"BigEndian\" or \"LittleEndian\""
            ),
            Error::InvalidType => write!(f, "Invalid VTKFile type detected"),
            //Error::InvalidAttributeValueFor(attrib) => {
            //    write!(f, "Invalid attribute value for {}", attrib)
            //}
            //Error::MissingAttribute(attrib) => write!(f, "Missing attribute: {}", attrib),
            Error::TypeExtensionMismatch => write!(
                f,
                "The extension of the VTK file doesn't match the type specified in the VTKFile tag"
            ),
            Error::UnexpectedElement(elem) => write!(f, "Unexpected XML Element: {}", elem),
            Error::Unknown => write!(f, "Internal error"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::XML(source) => Some(source),
            Error::Base64Decode(source) => Some(source),
            Error::Validation(source) => Some(source),
            Error::Model(source) => Some(source),
            Error::IO(source) => Some(source),
            Error::Deserialization(source) => Some(source),
            _ => None,
        }
    }
}

impl From<model::Error> for Error {
    fn from(e: model::Error) -> Error {
        Error::Model(e)
    }
}

impl From<base64::DecodeError> for Error {
    fn from(e: base64::DecodeError) -> Error {
        Error::Base64Decode(e)
    }
}

impl From<quick_xml::Error> for Error {
    fn from(e: quick_xml::Error) -> Error {
        Error::XML(e)
    }
}

impl From<ValidationError> for Error {
    fn from(e: ValidationError) -> Error {
        Error::Validation(e)
    }
}

impl From<de::DeError> for Error {
    fn from(e: de::DeError) -> Error {
        Error::Deserialization(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::IO(e)
    }
}

/// Module used to serialize and deserialize whitespace separated sequences of 6 integers.
mod extent {
    use super::Extent;
    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    struct ExtentVisitor;

    impl<'de> Visitor<'de> for ExtentVisitor {
        type Value = [i32; 6];

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a space separated sequence of 6 integers")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut iter = v.split_ascii_whitespace();
            let mut count = 0;
            let mut advance = |i: &mut std::str::SplitAsciiWhitespace| {
                let elem = i
                    .next()
                    .ok_or_else(|| de::Error::invalid_length(count, &self))?;
                count += 1;
                elem.parse()
                    .map_err(|e| de::Error::custom(format!("failed to parse integer: {}", e)))
            };
            Ok([
                advance(&mut iter)?,
                advance(&mut iter)?,
                advance(&mut iter)?,
                advance(&mut iter)?,
                advance(&mut iter)?,
                advance(&mut iter)?,
            ])
        }
    }

    impl Serialize for Extent {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let Extent([a, b, c, d, e, f]) = self;
            s.collect_str(&format_args!("{} {} {} {} {} {}", a, b, c, d, e, f))
        }
    }

    impl<'de> Deserialize<'de> for Extent {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(Extent(d.deserialize_str(ExtentVisitor)?))
        }
    }
}

/// Module used to serialize and deserialize whitespace separated sequences of 3 floats.
mod vector3 {
    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    struct Vector3Visitor;

    impl<'de> Visitor<'de> for Vector3Visitor {
        type Value = [f32; 3];

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a space separated sequence of 3 floats")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut iter = v.split_whitespace();
            let mut count = 0;
            let mut advance = |i: &mut std::str::SplitWhitespace| {
                let elem = i
                    .next()
                    .ok_or_else(|| de::Error::invalid_length(count, &self))?;
                count += 1;
                elem.parse()
                    .map_err(|e| de::Error::custom(format!("failed to parse float: {}", e)))
            };
            Ok([
                advance(&mut iter)?,
                advance(&mut iter)?,
                advance(&mut iter)?,
            ])
        }
    }

    pub struct Vector3(pub [f32; 3]);

    impl Serialize for Vector3 {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let Vector3([a, b, c]) = self;
            s.collect_str(&format_args!("{} {} {}", a, b, c))
        }
    }

    impl<'de> Deserialize<'de> for Vector3 {
        fn deserialize<D>(d: D) -> Result<Vector3, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_str(Vector3Visitor).map(Vector3)
        }
    }

    pub fn deserialize<'de, D>(d: D) -> Result<[f32; 3], D::Error>
    where
        D: Deserializer<'de>,
    {
        d.deserialize_str(Vector3Visitor)
    }
}

/// Module used to serialize and deserialize version numbers like `4.1` with a major and minor
/// parts.
mod version {
    use super::model::Version;
    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    struct VersionVisitor;

    impl<'de> Visitor<'de> for VersionVisitor {
        type Value = Version;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a dot separated pair of integers")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut iter = v.split('.');
            let advance = |i: &mut std::str::Split<'_, char>| {
                let elem = i
                    .next()
                    .ok_or_else(|| de::Error::custom("need a major and minor version numbers"))?;
                elem.parse()
                    .map_err(|e| de::Error::custom(format!("failed to parse version: {}", e)))
            };
            Ok(Version::new_xml(advance(&mut iter)?, advance(&mut iter)?))
        }
    }

    impl Serialize for Version {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let (major, minor) = self.to_xml();
            s.collect_str(&format_args!("{}.{}", major, minor))
        }
    }
    impl<'de> Deserialize<'de> for Version {
        fn deserialize<D>(d: D) -> Result<Version, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_str(VersionVisitor)
        }
    }
}

mod data {
    use super::{AppendedData, Data, Encoding, RawData};
    use serde::{
        de::{Deserialize, Deserializer, Visitor},
        Serialize, Serializer,
    };
    use std::fmt;

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        #[serde(rename = "@encoding")]
        Encoding,
        #[serde(rename = "$value")]
        Value,
    }

    impl Serialize for Data {
        fn serialize<S>(&self, se: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            match self {
                Data::Data(s) => se.serialize_str(s),
                _ => se.serialize_unit_variant("Data", 1, "InformationKey"),
            }
        }
    }

    /*
     * Data in an AppendedData element
     */

    struct RawDataVisitor;

    // Adopted from the nightly-only Rust standard library.
    #[cfg(feature = "binary")]
    #[inline]
    pub const fn trim_ascii_start(mut bytes: &[u8]) -> &[u8] {
        // Note: A pattern matching based approach (instead of indexing) allows
        // making the function const.
        while let [first, rest @ ..] = bytes {
            if first.is_ascii_whitespace() {
                bytes = rest;
            } else {
                break;
            }
        }
        bytes
    }

    #[cfg(feature = "binary")]
    fn trim_start_in_place(vec: &mut Vec<u8>) {
        let trimmed = trim_ascii_start(&vec);

        let trimmed_start_pointer = trimmed.as_ptr();
        let trimmed_length = trimmed.len();

        unsafe {
            core::ptr::copy(trimmed_start_pointer, vec.as_mut_ptr(), trimmed_length);

            vec.set_len(trimmed_length);
        }
    }

    impl<'de> Visitor<'de> for RawDataVisitor {
        type Value = RawData;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("Raw byte data")
        }

        // #[cfg(not(feature = "binary"))]
        fn visit_string<E>(self, mut v: String) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            use trim_in_place::TrimInPlace;
            v.trim_in_place();

            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData::default());
            }
            if !v.starts_with('_') {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }
            Ok(RawData(v[1..].to_string().into_bytes()))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            let v = v.trim();

            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData::default());
            }
            if !v.starts_with('_') {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }
            Ok(RawData(v[1..].to_string().into_bytes()))
        }

        fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            let v = v.trim();

            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData::default());
            }
            if !v.starts_with('_') {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }
            Ok(RawData(v[1..].to_string().into_bytes()))
        }

        #[cfg(feature = "binary")]
        fn visit_bytes<E: serde::de::Error>(self, v: &[u8]) -> Result<Self::Value, E> {
            let v = trim_ascii_start(v);

            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData::default());
            }
            if v[0] != b'_' {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }
            Ok(RawData(v[1..].to_vec()))
        }
        #[cfg(feature = "binary")]
        fn visit_byte_buf<E>(self, mut v: Vec<u8>) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            trim_start_in_place(&mut v);

            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData::default());
            }
            if v[0] != b'_' {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }

            v.remove(0);
            Ok(RawData(v))
        }
    }

    impl<'de> Deserialize<'de> for RawData {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            #[cfg(feature = "binary")]
            {
                d.deserialize_bytes(RawDataVisitor)
            }
            #[cfg(not(feature = "binary"))]
            {
                d.deserialize_str(RawDataVisitor)
            }
        }
    }

    enum EncodedRawData<'a> {
        Base64(&'a [u8]),
        #[cfg(feature = "binary")]
        Raw(&'a [u8]),
    }

    impl Serialize for EncodedRawData<'_> {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use serde::ser::Error;
            match self {
                EncodedRawData::Base64(data) => {
                    if !data.is_empty() {
                        let mut sbuf = String::from("_");
                        sbuf.push_str(&std::str::from_utf8(data).map_err(|err| {
                            S::Error::custom(format!(
                                "Invalid base64 encoding. UTF8 error: {}",
                                err
                            ))
                        })?);
                        s.serialize_str(&sbuf)
                    } else {
                        s.serialize_str("")
                    }
                }
                #[cfg(feature = "binary")]
                EncodedRawData::Raw(ref data) => {
                    if !data.is_empty() {
                        let mut v = Vec::with_capacity(data.len() + 1);
                        v.push(b'_');
                        v.extend_from_slice(&data);
                        s.serialize_bytes(&v)
                    } else {
                        s.serialize_bytes(&[])
                    }
                }
            }
        }
    }

    impl Serialize for AppendedData {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use serde::ser::SerializeStruct;
            let mut state = s.serialize_struct("AppendedData", 2)?;
            state.serialize_field("@encoding", &self.encoding)?;
            if matches!(self.encoding, Encoding::Base64) {
                state.serialize_field("$value", &EncodedRawData::Base64(&self.data.0))?;
            } else {
                #[cfg(feature = "binary")]
                state.serialize_field("$value", &EncodedRawData::Raw(&self.data.0))?;
            }
            state.end()
        }
    }
}

mod topo {
    use super::{Cells, DataArray, Topo};
    use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
    use std::fmt;

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        DataArray,
    }

    struct TopoVisitor;

    impl<'de> Visitor<'de> for TopoVisitor {
        type Value = Topo;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("topo data")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let make_err = || {
                <A::Error as serde::de::Error>::custom(
                "Topo data arrays must contain two DataArrays named \"connectivity\" and \"offsets\""
            )
            };
            let mut connectivity = None;
            let mut offsets = None;
            while let Some((_, field)) = map.next_entry::<Field, DataArray>()? {
                match field.name.as_str() {
                    "connectivity" => connectivity = Some(field),
                    "offsets" => offsets = Some(field),
                    _ => return Err(make_err()),
                }
            }
            let connectivity = connectivity.ok_or_else(make_err)?;
            let offsets = offsets.ok_or_else(make_err)?;
            Ok(Topo {
                connectivity,
                offsets,
            })
        }
    }

    impl<'de> Deserialize<'de> for Topo {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_struct("Topo", &["DataArray"; 2], TopoVisitor)
        }
    }

    struct CellsVisitor;

    impl<'de> Visitor<'de> for CellsVisitor {
        type Value = Cells;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("topo data")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let make_err = || {
                <A::Error as serde::de::Error>::custom(
                "Cells data arrays must contain three DataArrays named \"connectivity\", \"offsets\" and \"types\""
            )
            };
            let mut connectivity = None;
            let mut offsets = None;
            let mut types = None;
            while let Some((_, field)) = map.next_entry::<Field, DataArray>()? {
                match field.name.as_str() {
                    "connectivity" => connectivity = Some(field),
                    "offsets" => offsets = Some(field),
                    "types" => types = Some(field),
                    _ => return Err(make_err()),
                }
            }

            let connectivity = connectivity.ok_or_else(make_err)?;
            let offsets = offsets.ok_or_else(make_err)?;
            let types = types.ok_or_else(make_err)?;
            Ok(Cells {
                connectivity,
                offsets,
                types,
            })
        }
    }

    impl<'de> Deserialize<'de> for Cells {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_struct("Cells", &["DataArray"; 3], CellsVisitor)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Default, Deserialize, Serialize)]
#[serde(rename_all = "PascalCase")]
pub struct Element {
    #[serde(default, rename = "@info")]
    pub info: String,
    #[serde(default, rename = "$value")]
    pub data: Vec<Data>,
}

/*
 * The following defines the VTK XML model as Rust types, which is then serialized and deserialized
 * using serde.
 *
 * This model is exported in case users prefer to work with a bare bones VTK XML model without
 * additional handling of Legacy formats or on demand loading of "Parallel" XML files.
 */

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct VTKFile {
    #[serde(rename = "@type")]
    pub data_set_type: DataSetType,
    #[serde(rename = "@version")]
    pub version: model::Version,
    #[serde(rename = "@byte_order")]
    pub byte_order: model::ByteOrder,
    #[serde(rename = "@header_type", skip_serializing_if = "Option::is_none")]
    pub header_type: Option<ScalarType>, // Assumed to be UInt32 if missing
    #[serde(
        rename = "@compressor",
        default,
        skip_serializing_if = "Compressor::is_none"
    )]
    pub compressor: Compressor,
    #[serde(rename = "$value")]
    pub data_set: DataSet,
    #[serde(rename = "AppendedData", skip_serializing_if = "Option::is_none")]
    pub appended_data: Option<AppendedData>,
}

impl Default for VTKFile {
    fn default() -> VTKFile {
        VTKFile {
            data_set_type: DataSetType::ImageData,
            version: model::Version::new_xml(0, 1),
            byte_order: model::ByteOrder::BigEndian,
            header_type: None,
            compressor: Compressor::None,
            appended_data: None,
            data_set: DataSet::UnstructuredGrid(Unstructured { pieces: Vec::new() }),
        }
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub enum Compressor {
    #[serde(rename = "vtkLZ4DataCompressor")]
    LZ4,
    #[serde(rename = "vtkZLibDataCompressor")]
    ZLib,
    #[serde(rename = "vtkLZMADataCompressor")]
    LZMA,
    #[serde(other)]
    #[default]
    None,
}

impl Compressor {
    pub fn is_none(&self) -> bool {
        matches!(self, Compressor::None)
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum DataSet {
    ImageData(ImageData),
    PolyData(Unstructured),
    RectilinearGrid(Grid),
    StructuredGrid(Grid),
    UnstructuredGrid(Unstructured),
    PImageData(PImageData),
    PPolyData(PUnstructured),
    PRectilinearGrid(PRectilinearGrid),
    PStructuredGrid(PStructuredGrid),
    PUnstructuredGrid(PUnstructured),
}

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct ImageData {
    #[serde(rename = "@WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "@Origin", deserialize_with = "vector3::deserialize")]
    origin: [f32; 3],
    #[serde(rename = "@Spacing", deserialize_with = "vector3::deserialize")]
    spacing: [f32; 3],
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Grid {
    #[serde(rename = "@WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Unstructured {
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PImageData {
    #[serde(rename = "@GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "@WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "@Origin", deserialize_with = "vector3::deserialize")]
    origin: [f32; 3],
    #[serde(rename = "@Spacing", deserialize_with = "vector3::deserialize")]
    spacing: [f32; 3],
    #[serde(rename = "PPointData", skip_serializing_if = "Option::is_none")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData", skip_serializing_if = "Option::is_none")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PRectilinearGrid {
    #[serde(rename = "@GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "@WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "PPointData", skip_serializing_if = "Option::is_none")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData", skip_serializing_if = "Option::is_none")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "PCoordinates")]
    coordinates: PCoordinates,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PStructuredGrid {
    #[serde(rename = "@GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "@WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "PPointData", skip_serializing_if = "Option::is_none")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData", skip_serializing_if = "Option::is_none")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "PPoints")]
    points: PPoints,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PUnstructured {
    #[serde(rename = "@GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "PPointData")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "PPoints")]
    points: PPoints,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct PieceSource {
    #[serde(rename = "@Extent", skip_serializing_if = "Option::is_none")]
    extent: Option<Extent>,
    #[serde(rename = "@Source")]
    source: String,
}

/// Contents and attributes of the `PPointData` XML element.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
// #[serde(rename_all = "PascalCase")]
pub struct PAttributeData {
    #[serde(rename = "@Scalars", skip_serializing_if = "Option::is_none")]
    scalars: Option<String>,
    #[serde(rename = "@Vectors", skip_serializing_if = "Option::is_none")]
    vectors: Option<String>,
    #[serde(rename = "@Normals", skip_serializing_if = "Option::is_none")]
    normals: Option<String>,
    #[serde(rename = "@Tensors", skip_serializing_if = "Option::is_none")]
    tensors: Option<String>,
    #[serde(rename = "@TCoords", skip_serializing_if = "Option::is_none")]
    tcoords: Option<String>,
    #[serde(default)]
    data_array: Vec<PDataArray>,
}

impl PAttributeData {
    pub fn into_model_attributes_meta_data(self) -> Vec<model::ArrayMetaData> {
        let PAttributeData {
            scalars,
            vectors,
            normals,
            tensors,
            tcoords,
            data_array,
        } = self;

        let info = AttributeInfo {
            scalars,
            vectors,
            normals,
            tensors,
            tcoords,
        };

        data_array
            .into_iter()
            .filter_map(|x| x.into_model_array_meta_data(&info).ok())
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PCoordinates {
    #[serde(rename = "PDataArray")]
    pub pdata_array: [PDataArray; 3],
}

impl Default for PCoordinates {
    fn default() -> PCoordinates {
        let coord = PDataArray {
            scalar_type: ScalarType::Float32,
            name: String::new(),
            num_comp: 1,
        };
        PCoordinates {
            pdata_array: [coord.clone(), coord.clone(), coord],
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PPoints {
    #[serde(rename = "PDataArray")]
    data: PDataArray,
}

impl Default for PPoints {
    fn default() -> PPoints {
        PPoints {
            data: PDataArray {
                scalar_type: ScalarType::Float32,
                name: String::new(),
                num_comp: 3,
            },
        }
    }
}

/// The attribute on VTKFile indicating the contained data set type.
#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum DataSetType {
    ImageData,
    PolyData,
    RectilinearGrid,
    StructuredGrid,
    PImageData,
    PPolyData,
    PRectilinearGrid,
    PStructuredGrid,
    PUnstructuredGrid,
    #[serde(other)]
    UnstructuredGrid,
}

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Extent([i32; 6]);

impl From<model::Extent> for Extent {
    fn from(ext: model::Extent) -> Extent {
        let [x, y, z] = ext.into_ranges();
        Extent([
            *x.start(),
            *x.end(),
            *y.start(),
            *y.end(),
            *z.start(),
            *z.end(),
        ])
    }
}

impl From<Extent> for model::Extent {
    fn from(ext: Extent) -> model::Extent {
        let [x0, x1, y0, y1, z0, z1] = ext.0;
        model::Extent::Ranges([x0..=x1, y0..=y1, z0..=z1])
    }
}

// Helper for serializing number_of_cells
fn is_zero(n: &u32) -> bool {
    *n == 0
}

// Helper for serializing number_of_components
fn is_one(n: &u32) -> bool {
    *n == 1
}

#[derive(Clone, Debug, PartialEq, Default, Deserialize, Serialize)]
#[serde(rename_all = "PascalCase")]
pub struct Piece {
    #[serde(rename = "@Extent", default, skip_serializing_if = "Option::is_none")]
    pub extent: Option<Extent>,
    #[serde(rename = "@NumberOfPoints", default, skip_serializing_if = "is_zero")]
    pub number_of_points: u32,
    #[serde(rename = "@NumberOfVerts", default, skip_serializing_if = "is_zero")]
    pub number_of_verts: u32,
    #[serde(rename = "@NumberOfLines", default, skip_serializing_if = "is_zero")]
    pub number_of_lines: u32,
    #[serde(rename = "@NumberOfStrips", default, skip_serializing_if = "is_zero")]
    pub number_of_strips: u32,
    #[serde(rename = "@NumberOfPolys", default, skip_serializing_if = "is_zero")]
    pub number_of_polys: u32,
    #[serde(rename = "@NumberOfCells", default, skip_serializing_if = "is_zero")]
    pub number_of_cells: u32,
    #[serde(default, skip_serializing_if = "AttributeData::is_default")]
    pub point_data: AttributeData,
    #[serde(default, skip_serializing_if = "AttributeData::is_default")]
    pub cell_data: AttributeData,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub points: Option<Points>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cells: Option<Cells>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub verts: Option<Topo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lines: Option<Topo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strips: Option<Topo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub polys: Option<Topo>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coordinates: Option<Coordinates>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Points {
    #[serde(rename = "DataArray")]
    data: DataArray,
}

impl Points {
    pub fn from_io_buffer(buf: model::IOBuffer, ei: EncodingInfo) -> Result<Points> {
        Ok(Points {
            data: DataArray::from_io_buffer(buf, ei)?
                .with_num_comp(3)
                .with_name("Points"),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Cells {
    #[serde(rename = "DataArray")]
    connectivity: DataArray,
    #[serde(rename = "DataArray")]
    offsets: DataArray,
    #[serde(rename = "DataArray")]
    types: DataArray,
}

impl Cells {
    fn from_model_cells(cells: model::Cells, ei: EncodingInfo) -> Result<Cells> {
        let model::Cells { cell_verts, types } = cells;
        let (connectivity, offsets) = cell_verts.into_xml();
        Ok(Cells {
            connectivity: DataArray::from_io_buffer(connectivity.into(), ei)?
                .with_name("connectivity"),
            offsets: DataArray::from_io_buffer(offsets.into(), ei)?.with_name("offsets"),
            types: DataArray::from_io_buffer(
                types
                    .into_iter()
                    .map(|x| x as u8)
                    .collect::<model::IOBuffer>(),
                ei,
            )?
            .with_name("types"),
        })
    }

    /// Decodes a data array for types.
    ///
    /// These can be specified as u8 or some other integer type.
    /// This logic is encapsulated in this function.
    fn get_type_codes(
        buf: model::IOBuffer,
    ) -> std::result::Result<Vec<model::CellType>, ValidationError> {
        use num_traits::FromPrimitive;
        let type_codes = buf
            .cast_into::<u8>()
            .ok_or(ValidationError::InvalidDataFormat)?;
        type_codes
            .into_iter()
            .map(|x| model::CellType::from_u8(x).ok_or_else(|| ValidationError::InvalidCellType(x)))
            .collect()
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this `Topo` struct into a `mode::VertexNumbers` type.
    pub fn into_model_cells(
        self,
        l: usize,
        appended: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> std::result::Result<model::Cells, ValidationError> {
        let types = Self::get_type_codes(self.types.into_io_buffer(l, appended, ei)?)?;

        let offsets: Option<Vec<u64>> = self.offsets.into_io_buffer(l, appended, ei)?.cast_into();
        let offsets = offsets.ok_or(ValidationError::InvalidDataFormat)?;

        // Count the total number of vertices we expect in the connectivity array.
        let num_vertices: usize = offsets.last().map(|&x| x as usize).unwrap_or(0);

        let connectivity: Option<Vec<u64>> = self
            .connectivity
            .into_io_buffer(num_vertices, appended, ei)?
            .cast_into();
        let connectivity = connectivity.ok_or(ValidationError::InvalidDataFormat)?;
        Ok(model::Cells {
            cell_verts: model::VertexNumbers::XML {
                connectivity,
                offsets,
            },
            types,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Topo {
    #[serde(rename = "DataArray")]
    connectivity: DataArray,
    #[serde(rename = "DataArray")]
    offsets: DataArray,
}

impl Topo {
    /// Convert model topology type into `Topo`.
    fn from_model_topo(topo: model::VertexNumbers, ei: EncodingInfo) -> Result<Topo> {
        let (connectivity, offsets) = topo.into_xml();
        Ok(Topo {
            connectivity: DataArray::from_io_buffer(connectivity.into(), ei)?
                .with_name("connectivity"),
            offsets: DataArray::from_io_buffer(offsets.into(), ei)?.with_name("offsets"),
        })
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this `Topo` struct into a `mode::VertexNumbers` type.
    pub fn into_vertex_numbers(
        self,
        num_elements: usize,
        appended: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> std::result::Result<model::VertexNumbers, ValidationError> {
        let offsets: Option<Vec<u64>> = self
            .offsets
            .into_io_buffer(num_elements, appended, ei)?
            .cast_into();
        let offsets = offsets.ok_or(ValidationError::InvalidDataFormat)?;

        // Get the number of elements in the connectivity array from the last offset.
        let num_values = usize::try_from(*offsets.last().unwrap_or(&0))
            .map_err(|_| ValidationError::MissingTopologyOffsets)?;
        let connectivity: Option<Vec<u64>> = self
            .connectivity
            .into_io_buffer(num_values, appended, ei)?
            .cast_into();
        Ok(model::VertexNumbers::XML {
            connectivity: connectivity.ok_or(ValidationError::InvalidDataFormat)?,
            offsets,
        })
    }
}

/// Attribute data corresponding to the `PointData` or `CellData` elements.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct AttributeData {
    #[serde(rename = "@Scalars", skip_serializing_if = "Option::is_none")]
    pub scalars: Option<String>,
    #[serde(rename = "@Vectors", skip_serializing_if = "Option::is_none")]
    pub vectors: Option<String>,
    #[serde(rename = "@Normals", skip_serializing_if = "Option::is_none")]
    pub normals: Option<String>,
    #[serde(rename = "@Tensors", skip_serializing_if = "Option::is_none")]
    pub tensors: Option<String>,
    #[serde(rename = "@TCoords", skip_serializing_if = "Option::is_none")]
    pub tcoords: Option<String>,
    /// The (possibly empty) collection of data arrays representing individual attributes.
    #[serde(default)]
    pub data_array: Vec<DataArray>,
}

impl AttributeData {
    fn is_default(&self) -> bool {
        self == &AttributeData::default()
    }
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct AttributeInfo {
    #[serde(rename = "@Scalars")]
    pub scalars: Option<String>,
    #[serde(rename = "@Vectors")]
    pub vectors: Option<String>,
    #[serde(rename = "@Normals")]
    pub normals: Option<String>,
    #[serde(rename = "@Tensors")]
    pub tensors: Option<String>,
    #[serde(rename = "@TCoords")]
    pub tcoords: Option<String>,
}

impl AttributeInfo {
    /// Determine an appropriate `model::ElementType` for the given attribute name and number of components.
    pub fn element_type(&self, name: &str, num_comp: u32) -> model::ElementType {
        let AttributeInfo {
            scalars,
            vectors,
            normals,
            tensors,
            tcoords,
        } = self;

        // Pick the element type greedily.
        if let Some(scalars) = scalars {
            if scalars.as_str() == name {
                return model::ElementType::Scalars {
                    num_comp,
                    lookup_table: None,
                };
            }
        }
        if let Some(vectors) = vectors {
            if vectors.as_str() == name && num_comp == 3 {
                return model::ElementType::Vectors;
            }
        }
        if let Some(normals) = normals {
            if normals.as_str() == name && num_comp == 3 {
                return model::ElementType::Normals;
            }
        }
        if let Some(tensors) = tensors {
            if tensors.as_str() == name && num_comp == 9 {
                return model::ElementType::Tensors;
            }
        }
        if let Some(tcoords) = tcoords {
            if tcoords.as_str() == name && num_comp < 4 {
                return model::ElementType::TCoords(num_comp);
            }
        }

        model::ElementType::Generic(num_comp)
    }
}

impl AttributeData {
    pub fn from_model_attributes(attribs: Vec<model::Attribute>, ei: EncodingInfo) -> Result<Self> {
        let mut attribute_data = AttributeData::default();
        for attrib in attribs {
            if let model::Attribute::DataArray(data) = attrib {
                // Only pick the first found attribute as the active one.
                match data.elem {
                    model::ElementType::Scalars { .. } => {
                        if attribute_data.scalars.is_none() {
                            attribute_data.scalars = Some(data.name.to_string());
                        }
                    }
                    model::ElementType::Vectors => {
                        if attribute_data.vectors.is_none() {
                            attribute_data.vectors = Some(data.name.to_string());
                        }
                    }
                    model::ElementType::Normals => {
                        if attribute_data.normals.is_none() {
                            attribute_data.normals = Some(data.name.to_string());
                        }
                    }
                    model::ElementType::TCoords(_) => {
                        if attribute_data.tcoords.is_none() {
                            attribute_data.tcoords = Some(data.name.to_string());
                        }
                    }
                    model::ElementType::Tensors => {
                        if attribute_data.tensors.is_none() {
                            attribute_data.tensors = Some(data.name.to_string());
                        }
                    }
                    _ => {}
                }
                attribute_data
                    .data_array
                    .push(DataArray::from_model_data_array(data, ei)?);
            }
            // Field attributes are not supported, they are simply ignored.
        }
        Ok(attribute_data)
    }
    pub fn into_model_attributes(
        self,
        n: usize,
        appended_data: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> Vec<model::Attribute> {
        let AttributeData {
            scalars,
            vectors,
            normals,
            tensors,
            tcoords,
            data_array,
        } = self;

        let info = AttributeInfo {
            scalars,
            vectors,
            normals,
            tensors,
            tcoords,
        };

        data_array
            .into_iter()
            .filter_map(|x| x.into_attribute(n, appended_data, &info, ei).ok())
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Coordinates {
    #[serde(rename = "DataArray")]
    pub data_array: [DataArray; 3],
}

impl Coordinates {
    /// Construct `Coordinates` from `model::Coordinates`.
    pub fn from_model_coords(coords: model::Coordinates, ei: EncodingInfo) -> Result<Self> {
        Ok(Coordinates {
            data_array: [
                DataArray::from_io_buffer(coords.x, ei)?,
                DataArray::from_io_buffer(coords.y, ei)?,
                DataArray::from_io_buffer(coords.z, ei)?,
            ],
        })
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this struct into a `mode::Coordinates` type.
    pub fn into_model_coordinates(
        self,
        [nx, ny, nz]: [usize; 3],
        appended: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> std::result::Result<model::Coordinates, ValidationError> {
        let Coordinates {
            data_array: [x, y, z],
        } = self;
        let x = x.into_io_buffer(nx, appended, ei)?;
        let y = y.into_io_buffer(ny, appended, ei)?;
        let z = z.into_io_buffer(nz, appended, ei)?;
        Ok(model::Coordinates { x, y, z })
    }
}

/// A helper struct indicating how to read and write binary data stored in `DataArray`s.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct EncodingInfo {
    pub byte_order: model::ByteOrder,
    pub header_type: ScalarType,
    pub compressor: Compressor,
    // Note that compression level is meaningless during decoding.
    pub compression_level: u32,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PDataArray {
    #[serde(rename = "@type")]
    pub scalar_type: ScalarType,
    #[serde(rename = "@Name", default)]
    pub name: String,
    #[serde(
        rename = "@NumberOfComponents",
        default = "default_num_comp",
        skip_serializing_if = "is_one"
    )]
    pub num_comp: u32,
}

impl PDataArray {
    /// Convert this data array descriptor into a `model::ArrayMetaData` type.
    pub fn into_model_array_meta_data(
        self,
        info: &AttributeInfo,
    ) -> std::result::Result<model::ArrayMetaData, ValidationError> {
        let elem = info.element_type(&self.name, self.num_comp);
        Ok(model::ArrayMetaData {
            name: self.name,
            elem,
            scalar_type: self.scalar_type.into(),
        })
    }
}

fn deserialize_option_float<'de, D: serde::Deserializer<'de>>(
    d: D,
) -> std::result::Result<Option<f64>, D::Error> {
    let opt: Option<String> = Option::deserialize(d).unwrap_or_default();
    match opt.as_ref().map(String::as_str) {
        None | Some("") => Ok(None),
        Some(s) => str::parse::<f64>(s)
            .map(Some)
            .map_err(serde::de::Error::custom),
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct DataArray {
    #[serde(rename = "@type")]
    pub scalar_type: ScalarType,
    #[serde(rename = "@Name", default, skip_serializing_if = "String::is_empty")]
    pub name: String,
    #[serde(
        rename = "@NumberOfComponents",
        default = "default_num_comp",
        skip_serializing_if = "is_one"
    )]
    pub num_comp: u32,
    #[serde(rename = "@format")]
    pub format: DataArrayFormat,
    #[serde(rename = "@offset", skip_serializing_if = "Option::is_none")]
    pub offset: Option<u32>,
    #[serde(
        rename = "@RangeMin",
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "deserialize_option_float"
    )]
    pub range_min: Option<f64>,
    #[serde(
        rename = "@RangeMax",
        default,
        skip_serializing_if = "Option::is_none",
        deserialize_with = "deserialize_option_float"
    )]
    pub range_max: Option<f64>,
    #[serde(rename = "$value", default)]
    pub data: Vec<Data>,
}

// For dummy arrays useful in debugging.
impl Default for DataArray {
    fn default() -> DataArray {
        DataArray {
            scalar_type: ScalarType::UInt32,
            name: String::new(),
            format: DataArrayFormat::Binary,
            offset: None,
            num_comp: 1,
            range_min: None,
            range_max: None,
            data: vec![Data::default()],
        }
    }
}

impl DataArray {
    /// Construct a binary `DataArray` from a given `model::DataArray`.
    pub fn from_model_data_array(data: model::DataArray, ei: EncodingInfo) -> Result<Self> {
        let num_comp = u32::try_from(data.num_comp()).unwrap();
        Ok(DataArray {
            name: data.name,
            num_comp,
            ..DataArray::from_io_buffer(data.data, ei)?
        })
    }
    /// Construct a binary `DataArray` from a given `model::FieldArray`.
    pub fn from_field_array(field: model::FieldArray, ei: EncodingInfo) -> Result<Self> {
        Ok(DataArray {
            name: field.name,
            num_comp: field.elem,
            ..DataArray::from_io_buffer(field.data, ei)?
        })
    }
    /// Construct a binary `DataArray` from a given [`model::IOBuffer`].
    pub fn from_io_buffer(buf: model::IOBuffer, ei: EncodingInfo) -> Result<Self> {
        // Automatically determine the range for this buffer.
        let range = buf.compute_range();
        Ok(DataArray {
            scalar_type: buf.scalar_type().into(),
            range_min: range.map(|x| x.0),
            range_max: range.map(|x| x.1),
            data: vec![Data::Data({
                buf.into_bytes_with_size_encoded(ei, |x, s| BASE64_STANDARD.encode_string(x, s))?
            })],
            ..Default::default()
        })
    }

    /// Returns the given `DataArray` with name set to `name`.
    pub fn with_name(self, name: impl Into<String>) -> Self {
        DataArray {
            name: name.into(),
            ..self
        }
    }

    /// Returns the given `DataArray` with the given number of components `num_comp`.
    pub fn with_num_comp(self, num_comp: u32) -> Self {
        DataArray { num_comp, ..self }
    }

    /// Helper to extract possibly compressed binary data from a `String` in `IOBuffer` format.
    fn extract_data(
        ei: EncodingInfo,
        scalar_type: ScalarType,
        data: Data,
    ) -> std::result::Result<model::IOBuffer, ValidationError> {
        use model::IOBuffer;

        let header_bytes = ei.header_type.size();
        // Binary data in a data array (i.e. not in appended data) is always base64 encoded.
        // It can also be compressed.
        if matches!(ei.compressor, Compressor::None) {
            // First byte gives the bytes
            let bytes = BASE64_STANDARD.decode(data.into_string())?;
            // eprintln!("{:?}", &bytes[..header_bytes]);
            return Ok(IOBuffer::from_bytes(
                &bytes[header_bytes..],
                scalar_type.into(),
                ei.byte_order,
            )?);
        }

        // Temporary buffer used for decoding compressed types.
        let mut buf = Vec::new();

        let data_string = data.into_string();
        let encoded_data = data_string.as_bytes();
        let bytes = decode_and_decompress(
            &mut buf,
            base64_decode_buf,
            to_b64,
            encoded_data,
            header_bytes,
            ei,
        )?;

        Ok(IOBuffer::from_bytes(
            bytes.as_slice(),
            scalar_type.into(),
            ei.byte_order,
        )?)
    }

    /// Convert this data array into a `model::FieldArray` type.
    ///
    /// The given arguments are the number of elements (not bytes) in the expected output
    /// buffer and an optional appended data reference.
    pub fn into_field_array(
        self,
        l: usize,
        appended: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> std::result::Result<model::FieldArray, ValidationError> {
        use model::IOBuffer;

        let DataArray {
            name,
            scalar_type,
            format,
            offset,
            num_comp,
            data,
            ..
        } = self;

        //eprintln!("name = {:?}", &name);

        let num_elements = usize::try_from(num_comp).unwrap() * l;

        let data = match format {
            DataArrayFormat::Appended => {
                if let Some(appended) = appended {
                    let start: usize = offset.unwrap_or(0).try_into().unwrap();
                    let buf = appended.extract_data(start, num_elements, scalar_type, ei)?;
                    if buf.len() != num_elements {
                        return Err(ValidationError::DataArraySizeMismatch {
                            name,
                            expected: num_elements,
                            actual: buf.len(),
                        });
                    }
                    buf
                } else {
                    return Err(ValidationError::InvalidDataFormat);
                }
            }
            DataArrayFormat::Binary => {
                let buf = Self::extract_data(ei, scalar_type, data.into_iter().next().unwrap())?;
                if buf.len() != num_elements {
                    return Err(ValidationError::DataArraySizeMismatch {
                        name,
                        expected: num_elements,
                        actual: buf.len(),
                    });
                }
                buf
            }
            DataArrayFormat::Ascii => {
                let string = if data.is_empty() {
                    "".to_string()
                } else {
                    data[0].clone().into_string()
                };
                let slice = string.as_str();
                fn parse_num_seq<E, T>(s: &str) -> std::result::Result<Vec<T>, ValidationError>
                where
                    T: std::str::FromStr<Err = E>,
                    E: Into<ValidationError>,
                {
                    s.split_ascii_whitespace()
                        .map(|x| x.parse::<T>().map_err(Into::into))
                        .collect()
                }
                let buf = match scalar_type {
                    ScalarType::Int8 => IOBuffer::I8(parse_num_seq(slice)?),
                    ScalarType::UInt8 => IOBuffer::U8(parse_num_seq(slice)?),
                    ScalarType::Int16 => IOBuffer::I16(parse_num_seq(slice)?),
                    ScalarType::UInt16 => IOBuffer::U16(parse_num_seq(slice)?),
                    ScalarType::Int32 => IOBuffer::I32(parse_num_seq(slice)?),
                    ScalarType::UInt32 => IOBuffer::U32(parse_num_seq(slice)?),
                    ScalarType::Int64 => IOBuffer::I64(parse_num_seq(slice)?),
                    ScalarType::UInt64 => IOBuffer::U64(parse_num_seq(slice)?),
                    ScalarType::Float32 => IOBuffer::F32(parse_num_seq(slice)?),
                    ScalarType::Float64 => IOBuffer::F64(parse_num_seq(slice)?),
                };
                if buf.len() != num_elements {
                    return Err(ValidationError::DataArraySizeMismatch {
                        name,
                        expected: num_elements,
                        actual: buf.len(),
                    });
                }
                buf
            }
        };

        Ok(model::FieldArray {
            name,
            data,
            elem: num_comp,
        })
    }

    /// Convert this data array into a `model::DataArray` type.
    ///
    /// The given arguments are the number of elements (not bytes) in the expected output
    /// buffer and an optional appended data reference.
    pub fn into_model_data_array(
        self,
        l: usize,
        appended: Option<&AppendedData>,
        info: &AttributeInfo,
        ei: EncodingInfo,
    ) -> std::result::Result<model::DataArray, ValidationError> {
        // First convert into a field array.
        let model::FieldArray { name, data, elem } = self.into_field_array(l, appended, ei)?;

        // Then determine an appropriate element type.
        let elem = info.element_type(&name, elem);

        Ok(model::DataArray { name, data, elem })
    }

    /// Convert this data array into an `IOBuffer`.
    ///
    /// This is the same as `into_field_array` but only keeps the `IOBuffer` part.
    pub fn into_io_buffer(
        self,
        num_elements: usize,
        appended: Option<&AppendedData>,
        ei: EncodingInfo,
    ) -> std::result::Result<model::IOBuffer, ValidationError> {
        self.into_field_array(num_elements, appended, ei)
            .map(|model::FieldArray { data, .. }| data)
    }

    pub fn into_attribute(
        self,
        num_elements: usize,
        appended: Option<&AppendedData>,
        info: &AttributeInfo,
        ei: EncodingInfo,
    ) -> std::result::Result<model::Attribute, ValidationError> {
        let data_array = self.into_model_data_array(num_elements, appended, info, ei)?;
        Ok(model::Attribute::DataArray(data_array))
    }
}

fn default_num_comp() -> u32 {
    1
}

pub fn deserialize_string_trim<'de, D>(d: D) -> std::result::Result<String, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    use trim_in_place::TrimInPlace;
    let mut de_string = String::deserialize(d)?;
    de_string.trim_in_place();
    Ok(de_string)
}

/// The contents of a `DataArray` element.
///
/// Some VTK tools like ParaView may produce undocumented tags inside this
/// element. We capture and ignore those via the `Other` variant. Otherwise this
/// is treated as a data string.
#[derive(Clone, Debug, PartialEq, Deserialize)]
pub enum Data {
    #[serde(rename = "$text", deserialize_with = "deserialize_string_trim")]
    Data(String),
    #[serde(other)]
    Other,
}

impl Data {
    fn into_string(self) -> String {
        match self {
            Data::Other => String::new(),
            Data::Data(r) => r,
        }
    }
}

impl Default for Data {
    fn default() -> Data {
        Data::Data(String::default())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ScalarType {
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float32,
    #[serde(other)]
    Float64,
}

impl ScalarType {
    /// Returns the number of bytes of the corresponding scalar type.
    pub fn size(self) -> usize {
        use std::mem::size_of;
        match self {
            ScalarType::Int8 => size_of::<i8>(),
            ScalarType::UInt8 => size_of::<u8>(),
            ScalarType::Int16 => size_of::<i16>(),
            ScalarType::UInt16 => size_of::<u16>(),
            ScalarType::Int32 => size_of::<i32>(),
            ScalarType::UInt32 => size_of::<u32>(),
            ScalarType::Int64 => size_of::<i64>(),
            ScalarType::UInt64 => size_of::<u64>(),
            ScalarType::Float32 => size_of::<f32>(),
            ScalarType::Float64 => size_of::<f64>(),
        }
    }
}

impl From<model::ScalarType> for ScalarType {
    fn from(s: model::ScalarType) -> ScalarType {
        match s {
            model::ScalarType::Bit => ScalarType::UInt8,
            model::ScalarType::I8 => ScalarType::Int8,
            model::ScalarType::U8 => ScalarType::UInt8,
            model::ScalarType::I16 => ScalarType::Int16,
            model::ScalarType::U16 => ScalarType::UInt16,
            model::ScalarType::I32 => ScalarType::Int32,
            model::ScalarType::U32 => ScalarType::UInt32,
            model::ScalarType::I64 => ScalarType::Int64,
            model::ScalarType::U64 => ScalarType::UInt64,
            model::ScalarType::F32 => ScalarType::Float32,
            model::ScalarType::F64 => ScalarType::Float64,
        }
    }
}

impl From<ScalarType> for model::ScalarType {
    fn from(s: ScalarType) -> model::ScalarType {
        match s {
            ScalarType::Int8 => model::ScalarType::I8,
            ScalarType::UInt8 => model::ScalarType::U8,
            ScalarType::Int16 => model::ScalarType::I16,
            ScalarType::UInt16 => model::ScalarType::U16,
            ScalarType::Int32 => model::ScalarType::I32,
            ScalarType::UInt32 => model::ScalarType::U32,
            ScalarType::Int64 => model::ScalarType::I64,
            ScalarType::UInt64 => model::ScalarType::U64,
            ScalarType::Float32 => model::ScalarType::F32,
            ScalarType::Float64 => model::ScalarType::F64,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DataArrayFormat {
    Appended,
    Binary,
    Ascii,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct AppendedData {
    /// Encoding used in the `data` field.
    #[serde(rename = "@encoding")]
    pub encoding: Encoding,
    /// Raw data in binary or base64 format.
    ///
    /// The underscore present in the XML files is added and removed during
    /// serialization and deserialization respectively.
    #[serde(rename = "$value")]
    pub data: RawData,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct RawData(Vec<u8>);

/// Supported binary encoding formats.
#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Encoding {
    Base64,
    #[cfg(feature = "binary")]
    Raw,
}

const BASE64_ENGINE: base64::engine::GeneralPurpose = base64::engine::GeneralPurpose::new(
    &base64::alphabet::STANDARD,
    base64::engine::general_purpose::PAD.with_decode_allow_trailing_bits(true),
);

/// Customized base64::decode function that accepts a buffer.
fn base64_decode_buf<'a>(
    input: &[u8],
    buf: &'a mut Vec<u8>,
) -> std::result::Result<&'a [u8], ValidationError> {
    BASE64_ENGINE.decode_vec(input, buf)?;
    Ok(buf.as_slice())
}

/// Converts the number of target bytes to number of chars in base64 encoding.
fn to_b64(bytes: usize) -> usize {
    4 * (bytes as f64 / 3.0).ceil() as usize
    //(bytes * 4 + 1) / 3 + match bytes % 3 {
    //    1 => 2, 2 => 1, _ => 0
    //}
}

// Helper function to read a single header number, which depends on the encoding parameters.
fn read_header_num<R: AsRef<[u8]>>(
    header_buf: &mut std::io::Cursor<R>,
    ei: EncodingInfo,
) -> std::result::Result<usize, ValidationError> {
    use byteorder::ReadBytesExt;
    use byteorder::{BE, LE};
    Ok(match ei.byte_order {
        model::ByteOrder::LittleEndian => {
            if ei.header_type == ScalarType::UInt64 {
                header_buf.read_u64::<LE>()? as usize
            } else {
                header_buf.read_u32::<LE>()? as usize
            }
        }
        model::ByteOrder::BigEndian => {
            if ei.header_type == ScalarType::UInt64 {
                header_buf.read_u64::<BE>()? as usize
            } else {
                header_buf.read_u32::<BE>()? as usize
            }
        }
    })
}

// A trait for generalizing the decoder.
trait DecoderKit<'a> {
    fn read(&mut self, out: &mut [u8]) -> std::result::Result<usize, ValidationError>;
    fn make(input: &'a [u8]) -> Self;
}

#[cfg(feature = "flate2")]
impl<'a> DecoderKit<'a> for flate2::bufread::ZlibDecoder<&'a [u8]> {
    fn read(&mut self, out: &mut [u8]) -> std::result::Result<usize, ValidationError> {
        Ok(std::io::Read::read(self, out)?)
    }
    fn make(input: &'a [u8]) -> Self {
        flate2::bufread::ZlibDecoder::new(input)
    }
}

#[cfg(feature = "liblzma")]
impl<'a> DecoderKit<'a> for liblzma::read::XzDecoder<&'a [u8]> {
    fn read(&mut self, out: &mut [u8]) -> std::result::Result<usize, ValidationError> {
        Ok(std::io::Read::read(self, out)?)
    }
    fn make(input: &'a [u8]) -> Self {
        liblzma::read::XzDecoder::new(input)
    }
}

#[cfg(feature = "lz4")]
struct Lz4Decoder<'a>(&'a [u8]);

#[cfg(feature = "lz4")]
impl<'a> DecoderKit<'a> for Lz4Decoder<'a> {
    fn read(&mut self, out: &mut [u8]) -> std::result::Result<usize, ValidationError> {
        Ok(lz4::block::decompress_into(self.0, out)?)
    }
    fn make(input: &'a [u8]) -> Self {
        Lz4Decoder(input)
    }
}

fn decompress<'a, D>(
    [nb, nu, np]: [usize; 3],
    compressed_block_offsets: &[usize],
    decoded_data: &'a [u8],
) -> std::result::Result<Vec<u8>, ValidationError>
where
    D: DecoderKit<'a>,
{
    #[cfg(feature = "rayon")]
    use rayon::prelude::*;

    let mut out = vec![0u8; nu * (nb - 1) + np];
    #[cfg(feature = "rayon")]
    let blocks_iter = compressed_block_offsets.par_windows(2).map(|window| {
        let (a, b) = unsafe { (*window.get_unchecked(0), *window.get_unchecked(1)) };
        &decoded_data[a..b]
    });
    #[cfg(not(feature = "rayon"))]
    let blocks_iter = compressed_block_offsets.windows(2).map(|window| {
        let (a, b) = unsafe { (*window.get_unchecked(0), *window.get_unchecked(1)) };
        &decoded_data[a..b]
    });
    #[cfg(feature = "rayon")]
    if !blocks_iter
        .zip(out.par_chunks_mut(nu))
        .fold(
            || true,
            |mut acc, (block, out)| {
                let mut decoder = D::make(block);
                if let Ok(n) = decoder.read(out) {
                    acc &= n == out.len();
                } else {
                    return false;
                }
                acc
            },
        )
        .reduce(
            || true,
            |mut a, b| {
                a &= b;
                a
            },
        )
    {
        return Err(ValidationError::DecompressError);
    }
    #[cfg(not(feature = "rayon"))]
    for (block, out) in blocks_iter.zip(out.chunks_mut(nu)) {
        let mut decoder = D::make(block);
        if decoder.read(out)? != out.len() {
            return Err(ValidationError::DecompressError);
        }
    }
    Ok(out)
}

/// Returns an allocated decompressed Vec of bytes.
// Allow this warning which are fired when compression is disabled.
#[allow(unused_variables)]
fn decode_and_decompress<'a, D, B>(
    buf: &'a mut Vec<u8>,
    mut decode: D,
    mut to_b64: B,
    data: &'a [u8],
    header_bytes: usize,
    ei: EncodingInfo,
) -> std::result::Result<Vec<u8>, ValidationError>
where
    D: for<'b> FnMut(&'b [u8], &'b mut Vec<u8>) -> std::result::Result<&'b [u8], ValidationError>,
    B: FnMut(usize) -> usize,
{
    use std::io::Cursor;

    // Compressed data has a more complex header.
    // The data is organized as [nb][nu][np][nc_1]...[nc_nb][Data]
    // Where
    //   [nb] = Number of blocks in the data array
    //   [nu] = Block size before compression
    //   [np] = Size of the last partial block before compression (zero if it is not needed)
    //   [nc_i] = Size in bytes of block i after compression
    // See https://vtk.org/Wiki/VTK_XML_Formats for details.
    // In this case we don't know how many bytes are in the data array so we must first read
    // this information from a header.

    // First we need to determine the number of blocks stored.
    let num_blocks = {
        let encoded_header = &data[0..to_b64(header_bytes)];
        let decoded_header = decode(encoded_header, buf)?;
        read_header_num(&mut Cursor::new(decoded_header), ei)?
    };

    let full_header_bytes = header_bytes * (3 + num_blocks); // nb + nu + np + sum_i nc_i
    buf.clear();

    log::trace!("[decompress]: Num blocks: {:?}", num_blocks);
    log::trace!("[decompress]: header bytes: {:?}", header_bytes);
    log::trace!("[decompress]: full header bytes: {:?}", full_header_bytes);
    log::trace!(
        "[decompress]: full header bytes in base 64: {:?}",
        to_b64(full_header_bytes)
    );

    let encoded_header = &data[0..to_b64(full_header_bytes)];
    let decoded_header = decode(encoded_header, buf)?;
    log::trace!(
        "[decompress]: Decoded header length: {:?}",
        decoded_header.len()
    );
    let mut header_cursor = Cursor::new(decoded_header);
    let _nb = read_header_num(&mut header_cursor, ei)?; // We already know the number of blocks
    let nu = read_header_num(&mut header_cursor, ei)?; // Block size before compression
    log::trace!("[decompress]: Block size: {:?}", nu);
    let np = read_header_num(&mut header_cursor, ei)?; // Last block size before compression
    log::trace!("[decompress]: Last block size: {:?}", np);
    let compressed_block_offsets: Vec<_> = std::iter::once(0)
        .chain((0..num_blocks).scan(0, |state, _| {
            *state += read_header_num(&mut header_cursor, ei).unwrap_or(0);
            Some(*state)
        }))
        .collect();
    let nc_total = *compressed_block_offsets.last().unwrap_or(&0);
    log::trace!(
        "[decompress]: Compressed offsets: {:?}",
        compressed_block_offsets
    );
    log::trace!("[decompress]: Total number of bytes: {:?}", nc_total);
    let num_data_bytes = to_b64(nc_total);
    let start = to_b64(full_header_bytes);
    buf.clear();
    let encoded_data = &data[start..start + num_data_bytes];
    let decoded_data = decode(encoded_data, buf)?;

    // Now that the data is decoded, what is left is to decompress it.
    match ei.compressor {
        Compressor::ZLib => {
            #[cfg(not(feature = "flate2"))]
            {
                return Err(ValidationError::MissingCompressionLibrary(ei.compressor));
            }
            #[cfg(feature = "flate2")]
            {
                decompress::<flate2::bufread::ZlibDecoder<&'a [u8]>>(
                    [num_blocks, nu, np],
                    &compressed_block_offsets,
                    decoded_data,
                )
            }
        }
        Compressor::LZ4 => {
            #[cfg(not(feature = "lz4"))]
            {
                return Err(ValidationError::MissingCompressionLibrary(ei.compressor));
            }
            #[cfg(feature = "lz4")]
            {
                decompress::<Lz4Decoder>(
                    [num_blocks, nu, np],
                    &compressed_block_offsets,
                    decoded_data,
                )
            }
        }
        Compressor::LZMA => {
            #[cfg(not(feature = "liblzma"))]
            {
                return Err(ValidationError::MissingCompressionLibrary(ei.compressor));
            }
            #[cfg(feature = "liblzma")]
            {
                decompress::<liblzma::read::XzDecoder<&'a [u8]>>(
                    [num_blocks, nu, np],
                    &compressed_block_offsets,
                    decoded_data,
                )
            }
        }
        _ => {
            unreachable!()
        }
    }
}

impl AppendedData {
    /// Extract the decompressed and unencoded raw bytes from appended data.
    ///
    /// The data is expected to begin at `offset` from the beginning of the stored data array.
    ///
    /// The expected number of elements is given by `num_elements`.
    /// The given encoding info specifies the format of the data header and how the data is compressed.
    pub fn extract_data(
        &self,
        offset: usize,
        num_elements: usize,
        scalar_type: ScalarType,
        ei: EncodingInfo,
    ) -> std::result::Result<model::IOBuffer, ValidationError> {
        let header_bytes = ei.header_type.size();
        let expected_num_bytes = num_elements * scalar_type.size();
        let start = offset;

        if ei.compressor == Compressor::None {
            return match self.encoding {
                #[cfg(feature = "binary")]
                Encoding::Raw => {
                    // The first 64/32 bits gives the size of each component in bytes.
                    let given_num_bytes = read_header_num(
                        &mut std::io::Cursor::new(&self.data.0[start..start + header_bytes]),
                        ei,
                    )?;
                    if given_num_bytes != expected_num_bytes {
                        return Err(ValidationError::UnexpectedBytesInAppendedData(
                            expected_num_bytes as u64,
                            given_num_bytes as u64,
                        ));
                    }
                    let start = start + header_bytes;
                    let bytes = &self.data.0[start..start + expected_num_bytes];
                    Ok(model::IOBuffer::from_bytes(
                        bytes,
                        scalar_type.into(),
                        ei.byte_order,
                    )?)
                }
                Encoding::Base64 => {
                    // Add one integer that specifies the size of each component in bytes.
                    let num_target_bytes = expected_num_bytes + header_bytes;
                    // Compute how many base64 chars we need to decode l elements.
                    let num_source_bytes = to_b64(num_target_bytes);
                    let bytes = &self.data.0[start..start + num_source_bytes];
                    let bytes = BASE64_STANDARD.decode(bytes)?;
                    Ok(model::IOBuffer::from_bytes(
                        &bytes[header_bytes..],
                        scalar_type.into(),
                        ei.byte_order,
                    )?)
                }
            };
        }

        let out = match self.encoding {
            #[cfg(feature = "binary")]
            Encoding::Raw => {
                let mut buf = Vec::new();
                decode_and_decompress(
                    &mut buf,
                    |header, _| Ok(header),
                    |x| x,
                    &self.data.0[offset..],
                    header_bytes,
                    ei,
                )?
            }
            Encoding::Base64 => {
                let mut buf = Vec::new();
                decode_and_decompress(
                    &mut buf,
                    base64_decode_buf,
                    to_b64,
                    &self.data.0[offset..],
                    header_bytes,
                    ei,
                )?
            }
        };
        Ok(model::IOBuffer::from_byte_vec(
            out,
            scalar_type.into(),
            ei.byte_order,
        )?)
    }
}

/// A file type descriptor of a XML VTK data file.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FileType {
    storage: StorageFormat,
    data: DataType,
}

impl FileType {
    pub fn try_from_ext(ext: &str) -> Option<FileType> {
        Some(match ext {
            "vti" => FileType {
                storage: StorageFormat::Serial,
                data: DataType::ImageData,
            },
            "vtp" => FileType {
                storage: StorageFormat::Serial,
                data: DataType::PolyData,
            },
            "vtr" => FileType {
                storage: StorageFormat::Serial,
                data: DataType::RectilinearGrid,
            },
            "vts" => FileType {
                storage: StorageFormat::Serial,
                data: DataType::StructuredGrid,
            },
            "vtu" => FileType {
                storage: StorageFormat::Serial,
                data: DataType::UnstructuredGrid,
            },
            "pvti" => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::ImageData,
            },
            "pvtp" => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::PolyData,
            },
            "pvtr" => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::RectilinearGrid,
            },
            "pvts" => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::StructuredGrid,
            },
            "pvtu" => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::UnstructuredGrid,
            },
            _ => return None,
        })
    }
}

impl From<&DataSet> for DataSetType {
    fn from(d: &DataSet) -> DataSetType {
        match d {
            DataSet::ImageData(_) => DataSetType::ImageData,
            DataSet::PolyData(_) => DataSetType::PolyData,
            DataSet::RectilinearGrid(_) => DataSetType::RectilinearGrid,
            DataSet::StructuredGrid(_) => DataSetType::StructuredGrid,
            DataSet::UnstructuredGrid(_) => DataSetType::UnstructuredGrid,
            DataSet::PImageData(_) => DataSetType::PImageData,
            DataSet::PPolyData(_) => DataSetType::PPolyData,
            DataSet::PRectilinearGrid(_) => DataSetType::PRectilinearGrid,
            DataSet::PStructuredGrid(_) => DataSetType::PStructuredGrid,
            DataSet::PUnstructuredGrid(_) => DataSetType::PUnstructuredGrid,
        }
    }
}

impl From<DataSetType> for FileType {
    fn from(t: DataSetType) -> FileType {
        match t {
            DataSetType::ImageData => FileType {
                storage: StorageFormat::Serial,
                data: DataType::ImageData,
            },
            DataSetType::PolyData => FileType {
                storage: StorageFormat::Serial,
                data: DataType::PolyData,
            },
            DataSetType::RectilinearGrid => FileType {
                storage: StorageFormat::Serial,
                data: DataType::RectilinearGrid,
            },
            DataSetType::StructuredGrid => FileType {
                storage: StorageFormat::Serial,
                data: DataType::StructuredGrid,
            },
            DataSetType::UnstructuredGrid => FileType {
                storage: StorageFormat::Serial,
                data: DataType::UnstructuredGrid,
            },
            DataSetType::PImageData => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::ImageData,
            },
            DataSetType::PPolyData => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::PolyData,
            },
            DataSetType::PRectilinearGrid => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::RectilinearGrid,
            },
            DataSetType::PStructuredGrid => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::StructuredGrid,
            },
            DataSetType::PUnstructuredGrid => FileType {
                storage: StorageFormat::Parallel,
                data: DataType::UnstructuredGrid,
            },
        }
    }
}

/// The storage format of a given XML VTK file.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum StorageFormat {
    Parallel,
    Serial,
}

/// A data type representing particular structured or unstructured data.
///
/// Each of these can be stored either in Parallel or Serial format.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum DataType {
    ImageData,
    PolyData,
    RectilinearGrid,
    StructuredGrid,
    UnstructuredGrid,
}

impl DataType {
    pub fn validate(&self, fmt: &[u8]) -> Result<()> {
        if match self {
            DataType::ImageData => fmt == b"ImageData",
            DataType::PolyData => fmt == b"PolyData",
            DataType::RectilinearGrid => fmt == b"RectilinearGrid",
            DataType::StructuredGrid => fmt == b"StructuredGrid",
            DataType::UnstructuredGrid => fmt == b"UnstructuredGrid",
        } {
            Ok(())
        } else {
            Err(Error::TypeExtensionMismatch)
        }
    }

    pub fn try_from_byte_str(ty: &[u8]) -> Option<DataType> {
        Some(match ty {
            b"ImageData" => DataType::ImageData,
            b"PolyData" => DataType::PolyData,
            b"RectilinearGrid" => DataType::RectilinearGrid,
            b"StructuredGrid" => DataType::StructuredGrid,
            b"Unstructured" => DataType::UnstructuredGrid,
            _ => return None,
        })
    }
}

/*
 * Implement conversion from VTKFile which is almost verbatim what the XML file represents, and
 * Vtk, which is the unified model supporting Legacy format as well as loading of referenced
 * pieces.
 */

#[derive(Debug)]
pub enum ValidationError {
    MissingDataSet,
    DataSetMismatch,
    InvalidDataFormat,
    IO(std::io::Error),
    Model(model::Error),
    ParseFloat(std::num::ParseFloatError),
    ParseInt(std::num::ParseIntError),
    InvalidCellType(u8),
    TooManyElements(u32),
    UnexpectedBytesInAppendedData(u64, u64),
    MissingTopologyOffsets,
    MissingReferencedAppendedData,
    MissingCoordinates,
    MissingCompressionLibrary(Compressor),
    DataArraySizeMismatch {
        name: String,
        expected: usize,
        actual: usize,
    },
    Base64Decode(base64::DecodeError),
    Deserialize(de::DeError),
    #[cfg(feature = "lz4")]
    LZ4DecompressError(lz4::block::DecompressError),
    DecompressError,
    Unsupported,
}

#[cfg(feature = "lz4")]
impl From<lz4::block::DecompressError> for ValidationError {
    fn from(e: lz4::block::DecompressError) -> ValidationError {
        ValidationError::LZ4DecompressError(e)
    }
}

impl From<std::io::Error> for ValidationError {
    fn from(e: std::io::Error) -> ValidationError {
        ValidationError::IO(e)
    }
}

impl From<model::Error> for ValidationError {
    fn from(e: model::Error) -> ValidationError {
        ValidationError::Model(e)
    }
}

impl From<std::num::ParseFloatError> for ValidationError {
    fn from(e: std::num::ParseFloatError) -> ValidationError {
        ValidationError::ParseFloat(e)
    }
}

impl From<std::num::ParseIntError> for ValidationError {
    fn from(e: std::num::ParseIntError) -> ValidationError {
        ValidationError::ParseInt(e)
    }
}

impl From<base64::DecodeError> for ValidationError {
    fn from(e: base64::DecodeError) -> ValidationError {
        ValidationError::Base64Decode(e)
    }
}

impl From<de::DeError> for ValidationError {
    fn from(e: de::DeError) -> ValidationError {
        ValidationError::Deserialize(e)
    }
}

impl std::error::Error for ValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ValidationError::IO(source) => Some(source),
            ValidationError::Model(source) => Some(source),
            ValidationError::Base64Decode(source) => Some(source),
            ValidationError::Deserialize(source) => Some(source),
            ValidationError::ParseFloat(source) => Some(source),
            ValidationError::ParseInt(source) => Some(source),
            #[cfg(feature = "lz4")]
            ValidationError::LZ4DecompressError(source) => Some(source),
            _ => None,
        }
    }
}
impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValidationError::MissingDataSet => write!(f, "Missing a data set definition"),
            ValidationError::DataSetMismatch => {
                write!(f, "VTKFile type doesn't match internal data set definition")
            }
            ValidationError::InvalidDataFormat => write!(f, "Invalid data format"),
            ValidationError::IO(e) => write!(f, "IO Error: {}", e),
            ValidationError::Model(e) => write!(f, "Failed to convert model to xml: {}", e),
            ValidationError::ParseFloat(e) => write!(f, "Failed to parse a float: {}", e),
            ValidationError::ParseInt(e) => write!(f, "Failed to parse an int: {}", e),
            ValidationError::InvalidCellType(t) => write!(f, "Invalid cell type: {}", t),
            ValidationError::TooManyElements(n) => write!(f, "Too many elements: {}", n),
            ValidationError::UnexpectedBytesInAppendedData(expected, actual) => write!(
                f,
                "Expected {} bytes in appended data array but found {} in header",
                expected, actual
            ),
            ValidationError::MissingTopologyOffsets => write!(f, "Missing topology offsets"),
            ValidationError::MissingReferencedAppendedData => {
                write!(f, "Appended data is referenced but missing from the file")
            }
            ValidationError::MissingCoordinates => {
                write!(f, "Missing coordinates in rectilinear grid definition")
            }
            ValidationError::MissingCompressionLibrary(c) => {
                write!(
                    f,
                    "Cannot compress/decompress data: {:?} compression is unsupported",
                    c
                )
            }
            ValidationError::DataArraySizeMismatch {
                name,
                expected,
                actual,
            } => write!(
                f,
                "Data array \"{}\" has {} elements, but should have {}",
                name, actual, expected
            ),
            ValidationError::Base64Decode(source) => write!(f, "Base64 decode error: {}", source),
            ValidationError::Deserialize(source) => {
                write!(f, "Failed to deserialize data: {:?}", source)
            }
            #[cfg(feature = "lz4")]
            ValidationError::LZ4DecompressError(source) => {
                write!(f, "LZ4 decompression error: {}", source)
            }
            ValidationError::DecompressError => write!(f, "Decompression error"),
            ValidationError::Unsupported => write!(f, "Unsupported data set format"),
        }
    }
}

impl TryFrom<VTKFile> for model::Vtk {
    type Error = Error;
    fn try_from(xml: VTKFile) -> std::result::Result<model::Vtk, Self::Error> {
        let VTKFile {
            version,
            byte_order,
            compressor,
            header_type,
            data_set_type,
            appended_data,
            data_set,
            ..
        } = xml;

        let encoding_info = EncodingInfo {
            byte_order,
            header_type: header_type.unwrap_or(ScalarType::UInt32),
            compressor,
            compression_level: 0, // This is meaningless when decoding
        };

        let appended_data = appended_data.as_ref();

        // Validate that the expected data set type corresponds to the actual
        // stored data set.
        if data_set_type != DataSetType::from(&data_set) {
            return Err(ValidationError::DataSetMismatch.into());
        }

        // Convenience function to convert u32 to usize without panics.
        let convert_num = |n: u32| -> std::result::Result<usize, ValidationError> {
            usize::try_from(n).map_err(|_| ValidationError::TooManyElements(n))
        };

        let attributes =
            |npts, ncells, point_data: AttributeData, cell_data: AttributeData| model::Attributes {
                point: point_data.into_model_attributes(npts, appended_data, encoding_info),
                cell: cell_data.into_model_attributes(ncells, appended_data, encoding_info),
            };

        let data = match data_set {
            DataSet::ImageData(ImageData {
                whole_extent,
                origin,
                spacing,
                pieces,
            }) => model::DataSet::ImageData {
                extent: whole_extent.into(),
                origin,
                spacing,
                meta: None,
                pieces: pieces
                    .into_iter()
                    .map(
                        |Piece {
                             extent,
                             point_data,
                             cell_data,
                             ..
                         }| {
                            let extent: model::Extent = extent.unwrap_or(whole_extent).into();
                            let number_of_points = extent.num_points().try_into().unwrap();
                            let number_of_cells = extent.num_cells().try_into().unwrap();
                            model::Piece::Inline(Box::new(model::ImageDataPiece {
                                extent,
                                data: attributes(
                                    number_of_points,
                                    number_of_cells,
                                    point_data,
                                    cell_data,
                                ),
                            }))
                        },
                    )
                    .collect(),
            },
            DataSet::PolyData(Unstructured { pieces }) => model::DataSet::PolyData {
                meta: None,
                pieces: pieces
                    .into_iter()
                    .map(
                        |Piece {
                             number_of_points,
                             number_of_lines,
                             number_of_strips,
                             number_of_polys,
                             number_of_verts,
                             point_data,
                             cell_data,
                             points,
                             verts,
                             lines,
                             strips,
                             polys,
                             ..
                         }| {
                            let number_of_points = convert_num(number_of_points)?;
                            let number_of_lines = convert_num(number_of_lines)?;
                            let number_of_strips = convert_num(number_of_strips)?;
                            let number_of_polys = convert_num(number_of_polys)?;
                            let number_of_verts = convert_num(number_of_verts)?;
                            let number_of_cells = number_of_lines
                                + number_of_strips
                                + number_of_polys
                                + number_of_verts;
                            let verts = verts
                                .map(|verts| {
                                    verts.into_vertex_numbers(
                                        number_of_verts,
                                        appended_data,
                                        encoding_info,
                                    )
                                })
                                .transpose()?;
                            let lines = lines
                                .map(|lines| {
                                    lines.into_vertex_numbers(
                                        number_of_lines,
                                        appended_data,
                                        encoding_info,
                                    )
                                })
                                .transpose()?;
                            let strips = strips
                                .map(|strips| {
                                    strips.into_vertex_numbers(
                                        number_of_strips,
                                        appended_data,
                                        encoding_info,
                                    )
                                })
                                .transpose()?;
                            let polys = polys
                                .map(|polys| {
                                    polys.into_vertex_numbers(
                                        number_of_polys,
                                        appended_data,
                                        encoding_info,
                                    )
                                })
                                .transpose()?;
                            Ok(model::Piece::Inline(Box::new(model::PolyDataPiece {
                                points: points.unwrap().data.into_io_buffer(
                                    number_of_points,
                                    appended_data,
                                    encoding_info,
                                )?,
                                verts,
                                lines,
                                polys,
                                strips,
                                data: attributes(
                                    number_of_points,
                                    number_of_cells,
                                    point_data,
                                    cell_data,
                                ),
                            })))
                        },
                    )
                    .collect::<Result<Vec<model::Piece<model::PolyDataPiece>>>>()?,
            },
            DataSet::RectilinearGrid(Grid {
                whole_extent,
                pieces,
            }) => model::DataSet::RectilinearGrid {
                extent: whole_extent.into(),
                meta: None,
                pieces: pieces
                    .into_iter()
                    .map(
                        |Piece {
                             extent,
                             coordinates,
                             point_data,
                             cell_data,
                             ..
                         }| {
                            let extent: model::Extent = extent.unwrap_or(whole_extent).into();
                            let number_of_cells = extent.num_cells().try_into().unwrap();
                            let number_of_points = extent.num_points().try_into().unwrap();
                            let [nx, ny, nz] = extent.clone().into_dims();
                            let coords = coordinates.ok_or(ValidationError::MissingCoordinates)?;
                            let coords = coords.into_model_coordinates(
                                [nx as usize, ny as usize, nz as usize],
                                appended_data,
                                encoding_info,
                            )?;
                            Ok(model::Piece::Inline(Box::new(
                                model::RectilinearGridPiece {
                                    extent,
                                    coords,
                                    data: attributes(
                                        number_of_points,
                                        number_of_cells,
                                        point_data,
                                        cell_data,
                                    ),
                                },
                            )))
                        },
                    )
                    .collect::<Result<Vec<model::Piece<model::RectilinearGridPiece>>>>()?,
            },
            DataSet::StructuredGrid(Grid {
                whole_extent,
                pieces,
            }) => model::DataSet::StructuredGrid {
                extent: whole_extent.into(),
                meta: None,
                pieces: pieces
                    .into_iter()
                    .map(
                        |Piece {
                             extent,
                             points,
                             point_data,
                             cell_data,
                             ..
                         }| {
                            let extent: model::Extent = extent.unwrap_or(whole_extent).into();
                            let number_of_points = extent.num_points().try_into().unwrap();
                            let number_of_cells = extent.num_cells().try_into().unwrap();
                            Ok(model::Piece::Inline(Box::new(model::StructuredGridPiece {
                                extent,
                                points: points.unwrap().data.into_io_buffer(
                                    number_of_points,
                                    appended_data,
                                    encoding_info,
                                )?,
                                data: attributes(
                                    number_of_points,
                                    number_of_cells,
                                    point_data,
                                    cell_data,
                                ),
                            })))
                        },
                    )
                    .collect::<Result<Vec<model::Piece<model::StructuredGridPiece>>>>()?,
            },
            DataSet::UnstructuredGrid(Unstructured { pieces }) => {
                model::DataSet::UnstructuredGrid {
                    meta: None,
                    pieces: pieces
                        .into_iter()
                        .map(
                            |Piece {
                                 number_of_points,
                                 number_of_cells,
                                 point_data,
                                 cell_data,
                                 points,
                                 cells: topo,
                                 ..
                             }| {
                                let number_of_points = convert_num(number_of_points)?;
                                let number_of_cells = convert_num(number_of_cells)?;
                                let cells = topo
                                    .map(|topo| {
                                        topo.into_model_cells(
                                            number_of_cells,
                                            appended_data,
                                            encoding_info,
                                        )
                                    })
                                    .transpose()?
                                    .unwrap_or_default();
                                Ok(model::Piece::Inline(Box::new(
                                    model::UnstructuredGridPiece {
                                        points: points.unwrap().data.into_io_buffer(
                                            number_of_points,
                                            appended_data,
                                            encoding_info,
                                        )?,
                                        cells,
                                        data: attributes(
                                            number_of_points,
                                            number_of_cells,
                                            point_data,
                                            cell_data,
                                        ),
                                    },
                                )))
                            },
                        )
                        .collect::<Result<Vec<model::Piece<model::UnstructuredGridPiece>>>>()?,
                }
            }
            DataSet::PImageData(PImageData {
                ghost_level,
                whole_extent,
                origin,
                spacing,
                point_data,
                cell_data,
                pieces,
            }) => model::DataSet::ImageData {
                extent: whole_extent.into(),
                origin,
                spacing,
                meta: Some(Box::new(model::MetaData::ImageData {
                    ghost_level,
                    attributes: model::AttributesMetaData {
                        point_data: point_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                        cell_data: cell_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                    },
                })),
                pieces: pieces
                    .into_iter()
                    .map(|PieceSource { source, extent }| {
                        Ok(model::Piece::Source(source, extent.map(From::from)))
                    })
                    .collect::<Result<Vec<model::Piece<model::ImageDataPiece>>>>()?,
            },
            DataSet::PPolyData(PUnstructured {
                ghost_level,
                point_data,
                cell_data,
                points,
                pieces,
            }) => model::DataSet::PolyData {
                meta: Some(Box::new(model::MetaData::PolyData {
                    ghost_level,
                    points_type: points.data.scalar_type.into(),
                    attributes: model::AttributesMetaData {
                        point_data: point_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                        cell_data: cell_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                    },
                })),
                pieces: pieces
                    .into_iter()
                    .map(|PieceSource { source, extent }| {
                        Ok(model::Piece::Source(source, extent.map(From::from)))
                    })
                    .collect::<Result<Vec<model::Piece<model::PolyDataPiece>>>>()?,
            },
            DataSet::PRectilinearGrid(PRectilinearGrid {
                ghost_level,
                whole_extent,
                point_data,
                cell_data,
                coordinates,
                pieces,
            }) => model::DataSet::RectilinearGrid {
                extent: whole_extent.into(),
                meta: Some(Box::new(model::MetaData::RectilinearGrid {
                    ghost_level,
                    coords: [
                        coordinates.pdata_array[0].scalar_type.into(),
                        coordinates.pdata_array[1].scalar_type.into(),
                        coordinates.pdata_array[2].scalar_type.into(),
                    ],
                    attributes: model::AttributesMetaData {
                        point_data: point_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                        cell_data: cell_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                    },
                })),
                pieces: pieces
                    .into_iter()
                    .map(|PieceSource { source, extent }| {
                        Ok(model::Piece::Source(source, extent.map(From::from)))
                    })
                    .collect::<Result<Vec<model::Piece<model::RectilinearGridPiece>>>>()?,
            },
            DataSet::PStructuredGrid(PStructuredGrid {
                ghost_level,
                whole_extent,
                point_data,
                cell_data,
                points,
                pieces,
            }) => model::DataSet::StructuredGrid {
                extent: whole_extent.into(),
                meta: Some(Box::new(model::MetaData::StructuredGrid {
                    ghost_level,
                    points_type: points.data.scalar_type.into(),
                    attributes: model::AttributesMetaData {
                        point_data: point_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                        cell_data: cell_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                    },
                })),
                pieces: pieces
                    .into_iter()
                    .map(|PieceSource { source, extent }| {
                        Ok(model::Piece::Source(source, extent.map(From::from)))
                    })
                    .collect::<Result<Vec<model::Piece<model::StructuredGridPiece>>>>()?,
            },
            DataSet::PUnstructuredGrid(PUnstructured {
                ghost_level,
                point_data,
                cell_data,
                points,
                pieces,
            }) => model::DataSet::UnstructuredGrid {
                meta: Some(Box::new(model::MetaData::UnstructuredGrid {
                    ghost_level,
                    points_type: points.data.scalar_type.into(),
                    attributes: model::AttributesMetaData {
                        point_data: point_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                        cell_data: cell_data
                            .map(|x| x.into_model_attributes_meta_data())
                            .unwrap_or_default(),
                    },
                })),
                pieces: pieces
                    .into_iter()
                    .map(|PieceSource { source, extent }| {
                        Ok(model::Piece::Source(source, extent.map(From::from)))
                    })
                    .collect::<Result<Vec<model::Piece<model::UnstructuredGridPiece>>>>()?,
            },
        };

        Ok(model::Vtk {
            version,
            byte_order,
            title: String::new(),
            data,
            file_path: None,
        })
    }
}

impl model::Vtk {
    /// Converts the given Vtk model into an XML format represented by `VTKFile`.
    ///
    /// This function allows one to specify the compression level (0-9):
    /// ```verbatim
    /// 0 -> No compression
    /// 1 -> Fastest write
    /// ...
    /// 5 -> Balanced performance
    /// ...
    /// 9 -> Slowest but smallest file size.
    /// ```
    pub fn try_into_xml_format(
        self,
        mut compressor: Compressor,
        compression_level: u32,
    ) -> Result<VTKFile> {
        // Override compressor if level is set to 0 to avoid problems during reads.
        if compression_level == 0 {
            compressor = Compressor::None;
        }

        let model::Vtk {
            version,
            byte_order,
            data: data_set,
            file_path,
            ..
        } = self;

        let source_path = file_path.as_ref().map(|p| p.as_ref());

        let header_type = ScalarType::UInt64;

        let encoding_info = EncodingInfo {
            byte_order,
            header_type,
            compressor,
            compression_level,
        };

        let data_set = match data_set {
            model::DataSet::ImageData {
                extent,
                origin,
                spacing,
                pieces,
                //meta,
                ..
            } => DataSet::ImageData(ImageData {
                whole_extent: extent.into(),
                origin,
                spacing,
                pieces: pieces
                    .into_iter()
                    .map(|piece| {
                        let piece_data = piece.into_loaded_piece_data(source_path)?;
                        let model::ImageDataPiece { extent, data } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            point_data: AttributeData::from_model_attributes(
                                data.point,
                                encoding_info,
                            )?,
                            cell_data: AttributeData::from_model_attributes(
                                data.cell,
                                encoding_info,
                            )?,
                            ..Default::default()
                        })
                    })
                    .collect::<Result<Vec<Piece>>>()?,
            }),
            model::DataSet::StructuredGrid {
                extent,
                pieces,
                //meta,
                ..
            } => DataSet::StructuredGrid(Grid {
                whole_extent: extent.into(),
                pieces: pieces
                    .into_iter()
                    .map(|piece| {
                        let piece_data = piece.into_loaded_piece_data(source_path)?;
                        let model::StructuredGridPiece {
                            extent,
                            points,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            points: Some(Points::from_io_buffer(points, encoding_info)?),
                            point_data: AttributeData::from_model_attributes(
                                data.point,
                                encoding_info,
                            )?,
                            cell_data: AttributeData::from_model_attributes(
                                data.cell,
                                encoding_info,
                            )?,
                            ..Default::default()
                        })
                    })
                    .collect::<Result<Vec<Piece>>>()?,
            }),
            model::DataSet::RectilinearGrid {
                extent,
                pieces,
                //meta,
                ..
            } => DataSet::RectilinearGrid(Grid {
                whole_extent: extent.into(),
                pieces: pieces
                    .into_iter()
                    .map(|piece| {
                        let piece_data = piece.into_loaded_piece_data(source_path)?;
                        let model::RectilinearGridPiece {
                            extent,
                            coords,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            coordinates: Some(Coordinates::from_model_coords(
                                coords,
                                encoding_info,
                            )?),
                            point_data: AttributeData::from_model_attributes(
                                data.point,
                                encoding_info,
                            )?,
                            cell_data: AttributeData::from_model_attributes(
                                data.cell,
                                encoding_info,
                            )?,
                            ..Default::default()
                        })
                    })
                    .collect::<Result<Vec<Piece>>>()?,
            }),
            model::DataSet::UnstructuredGrid {
                pieces,
                //meta,
                ..
            } => DataSet::UnstructuredGrid(Unstructured {
                pieces: pieces
                    .into_iter()
                    .map(|piece| {
                        let piece_data = piece.into_loaded_piece_data(source_path)?;
                        let num_points = piece_data.num_points();
                        let model::UnstructuredGridPiece {
                            points,
                            cells,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            number_of_points: u32::try_from(num_points).unwrap(),
                            number_of_cells: u32::try_from(cells.num_cells()).unwrap(),
                            points: Some(Points::from_io_buffer(points, encoding_info)?),
                            cells: Some(Cells::from_model_cells(cells, encoding_info)?),
                            point_data: AttributeData::from_model_attributes(
                                data.point,
                                encoding_info,
                            )?,
                            cell_data: AttributeData::from_model_attributes(
                                data.cell,
                                encoding_info,
                            )?,
                            ..Default::default()
                        })
                    })
                    .collect::<Result<Vec<Piece>>>()?,
            }),
            model::DataSet::PolyData {
                pieces,
                //meta,
                ..
            } => DataSet::PolyData(Unstructured {
                pieces: pieces
                    .into_iter()
                    .map(|piece| {
                        let piece_data = piece.into_loaded_piece_data(source_path)?;
                        let num_points = piece_data.num_points();
                        let number_of_verts = piece_data.num_verts();
                        let number_of_lines = piece_data.num_lines();
                        let number_of_polys = piece_data.num_polys();
                        let number_of_strips = piece_data.num_strips();
                        let model::PolyDataPiece {
                            points,
                            verts,
                            lines,
                            polys,
                            strips,
                            data,
                        } = piece_data;

                        let verts = verts
                            .map(|topo| Topo::from_model_topo(topo, encoding_info))
                            .transpose()?;
                        let lines = lines
                            .map(|topo| Topo::from_model_topo(topo, encoding_info))
                            .transpose()?;
                        let polys = polys
                            .map(|topo| Topo::from_model_topo(topo, encoding_info))
                            .transpose()?;
                        let strips = strips
                            .map(|topo| Topo::from_model_topo(topo, encoding_info))
                            .transpose()?;

                        Ok(Piece {
                            number_of_points: u32::try_from(num_points).unwrap(),
                            number_of_lines: u32::try_from(number_of_lines).unwrap(),
                            number_of_verts: u32::try_from(number_of_verts).unwrap(),
                            number_of_polys: u32::try_from(number_of_polys).unwrap(),
                            number_of_strips: u32::try_from(number_of_strips).unwrap(),
                            points: Some(Points::from_io_buffer(points, encoding_info)?),
                            verts,
                            lines,
                            polys,
                            strips,
                            point_data: AttributeData::from_model_attributes(
                                data.point,
                                encoding_info,
                            )?,
                            cell_data: AttributeData::from_model_attributes(
                                data.cell,
                                encoding_info,
                            )?,
                            ..Default::default()
                        })
                    })
                    .collect::<Result<Vec<Piece>>>()?,
            }),
            model::DataSet::Field { data_array, .. } => {
                // Convert a legacy field data set into image data with a piece for each data_array.
                // If this becomes usefule we can try to group the arrays into
                // chunks of the same length and put those into a single piece
                // at different attributes.
                let max_count = data_array.iter().map(|v| v.len()).max().unwrap_or(0) as i32;
                DataSet::ImageData(ImageData {
                    whole_extent: Extent([0, max_count, 0, 0, 0, 0]),
                    origin: [0.0; 3],
                    spacing: [1.0; 3],
                    pieces: data_array
                        .into_iter()
                        .map(|data| {
                            Ok(Piece {
                                extent: Some(Extent([0, data.len() as i32, 0, 0, 0, 0])),
                                cell_data: AttributeData {
                                    data_array: vec![DataArray::from_field_array(
                                        data,
                                        encoding_info,
                                    )?],
                                    ..Default::default()
                                },
                                ..Default::default()
                            })
                        })
                        .collect::<Result<Vec<_>>>()?,
                })
            }
        };

        let data_set_type = DataSetType::from(&data_set);

        Ok(VTKFile {
            data_set_type,
            version,
            byte_order,
            header_type: Some(header_type),
            compressor,
            appended_data: None,
            data_set,
        })
    }
}

impl TryFrom<model::Vtk> for VTKFile {
    type Error = Error;
    fn try_from(vtk: model::Vtk) -> Result<VTKFile> {
        vtk.try_into_xml_format(Compressor::None, 0)
    }
}

// #[cfg(feature = "binary")]
// struct Context {
//     num: usize,
// }

// #[cfg(feature = "binary")]
// impl Context {
//     fn new() -> Self {
//         Context { num: 0 }
//     }
// }

// #[cfg(feature = "binary")]
// impl quick_xml::reader::Context for Context {
//     fn num_bytes_required(&mut self, look_ahead: &[u8]) -> usize {
//         // if look_ahead[0] == '_' {
//         //     let size = look_ahead[1];
//         // }
//         0
//     }
// }

impl VTKFile {
    /// Import an XML VTK file from the specified path.
    pub fn import(file_path: impl AsRef<Path>) -> Result<VTKFile> {
        let f = std::fs::File::open(file_path)?;
        Self::parse(std::io::BufReader::new(f))
    }

    /// Import an XML VTK file from the specified path.
    #[cfg(feature = "async")]
    pub async fn import_async(file_path: impl AsRef<Path>) -> Result<VTKFile> {
        let f = tokio::fs::File::open(file_path).await?;
        // Blocked on async support from quick-xml (e.g. https://github.com/tafia/quick-xml/pull/233)
        Ok(Self::parse(std::io::BufReader::new(f))?)
    }

    /// Parse an XML VTK file from the given reader.
    pub fn parse(reader: impl BufRead) -> Result<VTKFile> {
        // let mut reader = quick_xml::Reader::from_reader(reader);
        // let config = reader.config_mut();
        // config.expand_empty_elements = true;
        // config.check_end_names = true;
        // config.trim_text_start = true;
        // config.trim_text_end = false;
        // let mut de = de::Deserializer::from_custom_reader(reader);
        let mut de = de::Deserializer::from_reader(reader);
        Ok(VTKFile::deserialize(&mut de)?)
    }

    /// Export an XML VTK file to the specified path.
    pub fn export(&self, file_path: impl AsRef<Path>) -> Result<()> {
        let f = std::fs::File::create(file_path)?;
        self.write(std::io::BufWriter::new(f))
    }

    /// Write an XML VTK file to the specified writer.
    pub fn write(&self, writer: impl std::io::Write) -> Result<()> {
        se::to_writer(writer, self)?;
        Ok(())
    }
}

impl std::fmt::Display for VTKFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", se::to_string(self).map_err(|_| std::fmt::Error)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use quick_xml::reader::Reader;

    fn custom_reader(input: &str) -> Reader<&[u8]> {
        let mut reader = Reader::from_str(input);
        let config = reader.config_mut();
        config.expand_empty_elements = true;
        config.trim_text_end = true;
        config.trim_text_start = true;
        reader
    }

    fn from_str<T>(input: &str) -> std::result::Result<T, de::DeError>
    where
        T: serde::de::DeserializeOwned,
    {
        de::from_custom_reader(custom_reader(input))
    }

    // Helper to compress the human readable and indented examples below
    // into what we expect the serializer to output.
    fn compress_xml_str(s: &str) -> String {
        let compressed_str = regex::Regex::new(r"(?P<space>\s+<)")
            .unwrap()
            .replace_all(s, "<");
        regex::Regex::new(r">\s+(?P<n>(\d+|_))")
            .unwrap()
            .replace_all(&compressed_str, ">$n")
            .to_string()
    }

    #[test]
    fn piece_ser_de() {
        let piece_str = r#"
        <Piece Extent="0 1 0 1 0 1">
            <PointData Scalars="Temperature" Vectors="Velocity"/>
            <CellData Tensors="Stress"/>
        </Piece>"#;
        let piece: Piece = from_str(piece_str).unwrap();
        // eprintln!("{:#?}", &piece);
        let as_str = se::to_string(&piece).unwrap();
        assert_eq!(as_str, compress_xml_str(piece_str));
        let piece_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(piece, piece_roundtrip);
    }

    // Verify that image data struct works
    #[test]
    fn empty_image_data() {
        let image_data_str = r#" 
        <ImageData WholeExtent="0 1 0 1 0 1" Origin="0 0 0" Spacing="0.1 0.1 0.1">
            <Piece Extent="0 1 0 1 0 1">
                <PointData Scalars="Temperature" Vectors="Velocity"/>
                <CellData Tensors="Stress"/>
            </Piece>
        </ImageData>"#;

        let img: ImageData = from_str(image_data_str).unwrap();
        // eprintln!("{:#?}", &img);
        let as_str = se::to_string(&img).unwrap();
        assert_eq!(as_str, compress_xml_str(image_data_str));
        // eprintln!("{:?}", &as_str);
        let img_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(img, img_roundtrip);
    }

    // Verify that xmls with default empty meshes files work
    #[test]
    fn empty_image_data_file() {
        let vtk_str = r#" 
        <VTKFile type="ImageData" version="4.2" byte_order="BigEndian">
            <ImageData WholeExtent="0 1 0 1 0 1" Origin="0 0 0" Spacing="0.1 0.1 0.1">
                <Piece Extent="0 1 0 1 0 1">
                    <PointData Scalars="Temperature" Vectors="Velocity"/>
                    <CellData Tensors="Stress"/>
                </Piece>
            </ImageData>
        </VTKFile>"#;

        let vtk: VTKFile = from_str(vtk_str).unwrap();
        // eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        assert_eq!(as_str, compress_xml_str(vtk_str));
        // eprintln!("{:?}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
    }

    #[test]
    fn image_data() {
        let vtk_str = r#" 
        <VTKFile type="ImageData" version="4.2" byte_order="BigEndian">
        <ImageData WholeExtent="0 1 0 1 0 1" Origin="0 0 0" Spacing="0.1 0.1 0.1">
        <Piece Extent="0 1 0 1 0 1">
          <PointData Scalars="Temperature" Vectors="Velocity">
            <DataArray type="Float32" Name="Temperature" format="ascii">0 1 2 3 4 5 6 7 8 9</DataArray>
            <DataArray type="Float32" Name="Velocity" NumberOfComponents="3" format="ascii">
              0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
            </DataArray>
          </PointData>
        </Piece>
        </ImageData>
        </VTKFile>"#;

        let vtk: VTKFile = from_str(vtk_str).unwrap();
        // eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        assert_eq!(as_str, compress_xml_str(vtk_str));
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
    }

    #[test]
    fn topo() -> Result<()> {
        let polys = r#"<Topo>
          <DataArray type="Int32" Name="connectivity" format="ascii">
             0 1 2 3 4 5 6 7 0 1 5 4 2 3 7 6 0 4 7 3 1 2 6 5
          </DataArray>
          <DataArray type="Int32" Name="offsets" format="ascii">
             4 8 12 16 20 24
          </DataArray>
        </Topo>"#;

        let vtk: Topo = from_str(polys)?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk)?;
        assert_eq!(as_str, compress_xml_str(polys));
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn points() -> Result<()> {
        let points = r#"
        <Points>
          <DataArray type="Float32" NumberOfComponents="3" format="ascii">
            0 0 0 1 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 1 1 0 1 1
          </DataArray>
        </Points>"#;

        let vtk: Points = from_str(points)?;
        let as_str = se::to_string(&vtk)?;
        assert_eq!(as_str, compress_xml_str(points));
        let vtk_roundtrip = from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn simple_piece() -> Result<()> {
        let piece = r#"
        <Piece NumberOfPoints="8" NumberOfPolys="6">
        <PointData>
          <DataArray type="Float32" Name="my_scalars" format="ascii">
            0 1 2 3 4 5 6 7
         </DataArray>
        </PointData>
        </Piece>"#;

        let mut reader = quick_xml::reader::Reader::from_str(piece);
        let config = reader.config_mut();
        config.expand_empty_elements = true;
        config.trim_text_end = true;
        config.trim_text_start = true;
        let vtk: Piece = de::from_custom_reader(reader)?;
        // eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk)?;
        assert_eq!(as_str, compress_xml_str(piece));
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn piece() -> Result<()> {
        let piece = r#"
        <Piece NumberOfPoints="8" NumberOfPolys="6">
        <PointData Scalars="my_scalars">
          <DataArray type="Float32" Name="my_scalars" format="ascii">
            0 1 2 3 4 5 6 7
         </DataArray>
        </PointData>
        <CellData Scalars="cell_scalars" Normals="cell_normals">
          <DataArray type="Int32" Name="cell_scalars" format="ascii">
           0 1 2 3 4 5
          </DataArray>
          <DataArray type="Float32" Name="cell_normals" NumberOfComponents="3" format="ascii">
            0 0 -1 0 0 1 0 -1 0 0 1 0 -1 0 0 1 0 0
          </DataArray>
        </CellData>
        <Points>
          <DataArray type="Float32" NumberOfComponents="3" format="ascii">
            0 0 0 1 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 1 1 0 1 1
          </DataArray>
        </Points>
        <Polys>
          <DataArray type="Int32" Name="connectivity" format="ascii">
             0 1 2 3 4 5 6 7 0 1 5 4 2 3 7 6 0 4 7 3 1 2 6 5
          </DataArray>
          <DataArray type="Int32" Name="offsets" format="ascii">
             4 8 12 16 20 24
          </DataArray>
        </Polys>
        </Piece>"#;

        let vtk: Piece = from_str(piece)?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk)?;
        assert_eq!(as_str, compress_xml_str(piece));
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_ascii() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGrid_ascii.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_compressed_reduced() -> Result<()> {
        let vtk_str = r#"
            <VTKFile type="RectilinearGrid" version="1.0" byte_order="LittleEndian" header_type="UInt64" compressor="vtkZLibDataCompressor">
            <RectilinearGrid WholeExtent="0 3 0 1 0 1">
                <Piece Extent="0 3 0 1 0 1">
                <CellData Scalars="Pressure"/>
                </Piece>
            </RectilinearGrid>
            <AppendedData encoding="base64">
            _AQAAAAAAAAAAgAAAAAAAAAwAAAAAAAAACwAAAAAAAAA=eJxjYEAAAAAMAAE=AQAAAAAAAAAAgAAAAAAAAAwAAAAAAAAAEgAAAAAAAAA=eJxjYGiwZ2BgAOIGewAJvQG+AQAAAAAAAAAAgAAAAAAAAAwAAAAAAAAADwAAAAAAAAA=eJxjYIAAY+PN9gADFgFZAQAAAAAAAAAAgAAAAAAAAAwAAAAAAAAADQAAAAAAAAA=eJxjYICBBnsAAUsAwA==AQAAAAAAAAAAgAAAAAAAAAwAAAAAAAAADgAAAAAAAAA=eJxjYAADexABAAFHAEA=AQAAAAAAAAAAgAAAAAAAACAAAAAAAAAAGAAAAAAAAAA=eJxjYAABjgNgiuHDfihtD6E5HAA9JgPvAQAAAAAAAAAAgAAAAAAAABAAAAAAAAAADQAAAAAAAAA=eJxjYEAGH+wBAi8BMA==AQAAAAAAAAAAgAAAAAAAABAAAAAAAAAAEQAAAAAAAAA=eJxjYACBD/sZILQ9ABJGAt8=
            </AppendedData>
            </VTKFile>"#;
        let vtk: VTKFile = from_str(vtk_str)?;
        // eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        assert_eq!(as_str, compress_xml_str(vtk_str));
        // eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_compressed() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGridCompressed.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_appended_base64() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGridAppendedBase64.vtr")?;
        // eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        // eprintln!("{}", String::from_utf8_lossy(&as_bytes));
        let vtk_roundtrip = VTKFile::parse(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[cfg(feature = "binary")]
    #[test]
    fn rectilinear_grid_appended_raw_binary() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGridRawBinary.vtr")?;
        eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        eprintln!("{}", String::from_utf8_lossy(&as_bytes));
        let vtk_roundtrip = VTKFile::parse(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_inline_binary() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGridInlineBinary.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        //eprintln!("{:?}", &as_bytes);
        let vtk_roundtrip = VTKFile::parse(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn single_point() -> Result<()> {
        let inline_raw = VTKFile::import("assets/point.vtp")?;
        let vtk_inline_raw: model::Vtk = inline_raw.try_into()?;
        let vtk_ascii: model::Vtk = VTKFile::import("assets/point_ascii.vtp")?.try_into()?;
        assert_eq!(&vtk_inline_raw, &vtk_ascii);
        Ok(())
    }

    // Ensure that all binary formats produce identical (lossless) instances.
    #[cfg(feature = "binary")]
    #[test]
    fn rectilinear_grid_binary_all() -> Result<()> {
        let inline_raw = VTKFile::import("assets/RectilinearGridInlineBinary.vtr")?;
        let vtk_inline_raw: model::Vtk = inline_raw.try_into()?;
        let vtk_app_raw: model::Vtk =
            VTKFile::import("assets/RectilinearGridRawBinary.vtr")?.try_into()?;
        let vtk_app_base64: model::Vtk =
            VTKFile::import("assets/RectilinearGridAppendedBase64.vtr")?.try_into()?;
        //let vtk = VTKFile::import("assets/RectilinearGridCompressed.vtr")?;
        //let vtk = VTKFile::import("assets/RectilinearGrid.pvtr")?;
        assert_eq!(&vtk_inline_raw, &vtk_app_raw);
        assert_eq!(&vtk_inline_raw, &vtk_app_base64);
        Ok(())
    }

    #[test]
    fn parallel_rectilinear_grid() -> Result<()> {
        let vtk = VTKFile::import("assets/RectilinearGrid.pvtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn poly_cube() -> Result<()> {
        let vtk = VTKFile::import("assets/polyEx0.vtp")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn parallel_cube() -> Result<()> {
        let vtk = VTKFile::import("assets/cube.pvtp")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn hexahedron_appended() -> Result<()> {
        let vtk = VTKFile::import("assets/hexahedron.vtu")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn hexahedron_ascii() -> Result<()> {
        let vtk = VTKFile::import("assets/hexahedron_ascii.vtu")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn hexahedron_inline_binary() -> Result<()> {
        let vtk = VTKFile::import("assets/hexahedron_inline_binary.vtu")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        let vtk_ascii = VTKFile::import("assets/hexahedron_ascii.vtu")?;
        //eprintln!("{:#?}", &vtk_ascii);
        // Verify that the ascii cube is the same as the inline binary cube.
        let vtk_ascii = model::Vtk::try_from(vtk_ascii.clone())?;
        let vtk_binary = model::Vtk::try_from(vtk.clone())?;
        assert_eq!(&vtk_ascii, &vtk_binary);
        Ok(())
    }

    #[test]
    fn coordinates() -> Result<()> {
        let xml = r#"<Coordinates>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            </Coordinates>"#;
        let coords: Coordinates = from_str(xml)?;
        let xml_round_trip = se::to_string(&coords)?;
        let round_trip: Coordinates = from_str(&xml_round_trip)?;
        assert_eq!(round_trip, coords);
        Ok(())
    }

    #[test]
    fn data_array_appended() -> Result<()> {
        let appended_xml = "<DataArray type=\"Float32\" Name=\"vectors\" \
                            NumberOfComponents=\"3\" format=\"appended\" offset=\"0\" \
                            RangeMin=\"1.2\" RangeMax=\"4.3\"/>";
        let appended: DataArray = from_str(appended_xml)?;
        let appended_xml_round_trip = se::to_string(&appended)?;
        let appended_round_trip: DataArray = from_str(&appended_xml_round_trip)?;
        assert_eq!(appended_round_trip, appended);
        Ok(())
    }

    #[test]
    fn data_array_binary() -> Result<()> {
        let binary_xml = "<DataArray type=\"Float32\" Name=\"scalars\" format=\"binary\"> \
                          _bAAAAAAAAAAAAIA/AAAAQAAAQEAAAIBA </DataArray>";

        let binary: DataArray = from_str(binary_xml)?;
        //eprintln!("bin:{:?}", binary.data);
        let binary_xml_round_trip = se::to_string(&binary)?;
        let binary_round_trip: DataArray = from_str(&binary_xml_round_trip)?;
        assert_eq!(binary_round_trip, binary);
        Ok(())
    }

    #[test]
    fn data_array_ascii() -> Result<()> {
        let ascii_xml =
            "<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\"> 10 20 30 </DataArray>";

        let ascii: DataArray = from_str(ascii_xml)?;
        let ascii_xml_round_trip = se::to_string(&ascii)?;
        let ascii_round_trip: DataArray = from_str(&ascii_xml_round_trip)?;
        assert_eq!(ascii_round_trip, ascii);
        Ok(())
    }

    // An example of a data array produced by paraview.
    // The parser should effectively ignore unrecognized fields.
    #[test]
    fn data_array_paraview() -> Result<()> {
        let xml = r#"
          <DataArray type="Float32" Name="Points" NumberOfComponents="3" format="appended" RangeMin="0"                    RangeMax="1.7320508076"         offset="0"                   >
          <InformationKey name="L2_NORM_FINITE_RANGE" location="vtkDataArray" length="2">
            <Value index="0">
              0
            </Value>
            <Value index="1">
              1.7320508076
            </Value>
          </InformationKey>
          <InformationKey name="L2_NORM_RANGE" location="vtkDataArray" length="2">
            <Value index="0">
              0
            </Value>
            <Value index="1">
              1.7320508076
            </Value>
          </InformationKey>
        </DataArray>"#;

        let arr: DataArray = from_str(xml)?;
        let xml_round_trip = se::to_string(&arr)?;
        let round_trip: DataArray = from_str(&xml_round_trip)?;
        assert_eq!(round_trip, arr);
        Ok(())
    }

    /*
     * Test conversion facilities
     */

    #[test]
    fn xml_to_vtk_conversion() -> Result<()> {
        use model::*;
        let xml = VTKFile::import("assets/RectilinearGrid_ascii.vtr")?;
        let vtk: Vtk = xml.clone().try_into()?;
        assert_eq!(
            vtk,
            Vtk {
                version: Version::new_xml(1, 0),
                byte_order: ByteOrder::LittleEndian,
                title: String::new(),
                data: DataSet::inline(RectilinearGridPiece {
                    extent: Extent::Ranges([0..=3, 0..=1, 0..=1]),
                    coords: Coordinates {
                        x: vec![-3.0, -1.0, 1.0, 3.0].into(),
                        y: vec![0.0, 1.0].into(),
                        z: vec![-1.0, 1.0].into(),
                    },
                    data: Attributes {
                        point: Vec::new(),
                        cell: vec![
                            Attribute::scalars("Pressure", 1).with_data(vec![0.0f32, 0.0, 0.0]),
                            Attribute::generic("Void Volume Fraction", 1)
                                .with_data(vec![1.0f32, 0.5, 1.0]),
                            Attribute::generic("X Velocity", 1).with_data(vec![0.0f32, 0.0, 1.4]),
                            Attribute::generic("Y Velocity", 1).with_data(vec![0.0f32, 0.0, 1.0]),
                            Attribute::generic("Z Velocity", 1).with_data(vec![0.0f32, 0.5, 0.0]),
                        ]
                    }
                }),
                file_path: None,
            }
        );
        Ok(())
    }

    #[test]
    fn vtk_xml_conversion_round_trip() -> Result<()> {
        use model::*;
        let xml = VTKFile::import("assets/RectilinearGrid_ascii.vtr")?;
        let vtk: Vtk = xml.clone().try_into()?;
        let xml_round_trip: VTKFile = vtk.clone().try_into()?;
        let vtk_round_trip: Vtk = xml_round_trip.clone().try_into()?;
        assert_eq!(vtk.clone(), vtk_round_trip,);
        assert_eq!(xml_round_trip.clone(), vtk_round_trip.try_into()?);
        Ok(())
    }

    #[test]
    fn empty_polydata_topology() {
        use super::Data::Data;
        use super::DataArray as XMLDataArray;
        use crate::IOBuffer;
        let data = XMLDataArray {
            name: "PolyData".to_string(),
            scalar_type: ScalarType::Float32,
            format: DataArrayFormat::Ascii,
            offset: None,
            num_comp: 1,
            range_min: Some(0.1),
            range_max: Some(0.3),
            data: vec![Data("0.1 0.2 0.3".to_string())],
        };
        let data_empty = XMLDataArray {
            name: "PolyData".to_string(),
            scalar_type: ScalarType::Float32,
            format: DataArrayFormat::Ascii,
            offset: None,
            num_comp: 1,
            range_min: None,
            range_max: None,
            data: vec![],
        };
        let encoding_info = EncodingInfo {
            byte_order: model::ByteOrder::BigEndian,
            header_type: ScalarType::Float64,
            compressor: Compressor::None,
            compression_level: 0,
        };
        let data_fa = data.into_field_array(3, None, encoding_info);
        let data_empty_fa = data_empty.into_field_array(0, None, encoding_info);
        assert_eq!(data_fa.unwrap().data, IOBuffer::F32(vec![0.1, 0.2, 0.3]));
        assert_eq!(data_empty_fa.unwrap().data, IOBuffer::F32(vec![]));
    }
}
