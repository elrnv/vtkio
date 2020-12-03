//!
//! Internal APIs for dealing with XML VTK file types.
//!
//! See [VTK XML Format
//! Reference](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/#xml-file-formats) for
//! details on the xml format.
//!

mod se;

use quick_xml::de;
use std::convert::{TryFrom, TryInto};
use std::io::{BufRead, Write};
use std::path::Path;

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

/// Module used to serialize and deserialize the compressor enum.
mod compressor {
    use super::Compressor;
    use serde::de::{self, Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    struct CompressorVisitor;

    impl<'de> Visitor<'de> for CompressorVisitor {
        type Value = Compressor;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string identifying the vtk data compressor")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(match v {
                "vtkZLibDataCompressor" => Compressor::ZLib,
                "vtkLZ4DataCompressor" => Compressor::LZ4,
                "vtkLZMADataCompressor" => Compressor::LZMA,
                _ => Compressor::None,
            })
        }
    }

    impl Serialize for Compressor {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let compressor = match self {
                Compressor::ZLib => "vtkZLibDataCompressor",
                Compressor::LZ4 => "vtkLZ4DataCompressor",
                Compressor::LZMA => "vtkLZMADataCompressor",
                Compressor::None => return s.serialize_none(),
            };
            s.serialize_str(compressor)
        }
    }
    impl<'de> Deserialize<'de> for Compressor {
        fn deserialize<D>(d: D) -> Result<Compressor, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_str(CompressorVisitor)
        }
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
                let elem = i.next().ok_or(de::Error::invalid_length(count, &self))?;
                count += 1;
                elem.parse()
                    .map_err(|e| de::Error::custom(&format!("failed to parse integer: {}", e)))
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
                    .map_err(|e| de::Error::custom(&format!("failed to parse float: {}", e)))
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
                    .ok_or(de::Error::custom("need a major and minor version numbers"))?;
                elem.parse()
                    .map_err(|e| de::Error::custom(&format!("failed to parse version: {}", e)))
            };
            Ok(Version::new((advance(&mut iter)?, advance(&mut iter)?)))
        }
    }

    impl Serialize for Version {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let Version { major, minor } = self;
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

mod pcoordinates {
    use super::{PCoordinates, PDataArray};
    use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        PDataArray,
    }

    struct PCoordinatesVisitor;

    impl<'de> Visitor<'de> for PCoordinatesVisitor {
        type Value = PCoordinates;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("an array of 3 PDataArrays")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let invalid_len_err = |n| <A::Error as serde::de::Error>::invalid_length(n, &self);
            let (_, x) = map
                .next_entry::<Field, PDataArray>()?
                .ok_or_else(|| invalid_len_err(0))?;
            let (_, y) = map
                .next_entry::<Field, PDataArray>()?
                .ok_or_else(|| invalid_len_err(1))?;
            let (_, z) = map
                .next_entry::<Field, PDataArray>()?
                .ok_or_else(|| invalid_len_err(2))?;
            Ok(PCoordinates([x, y, z]))
        }
    }

    impl Serialize for PCoordinates {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use serde::ser::SerializeStruct;
            let PCoordinates([x, y, z]) = self;
            let mut ss = s.serialize_struct("PCoordinates", 3)?;
            ss.serialize_field("PDataArray", x)?;
            ss.serialize_field("PDataArray", y)?;
            ss.serialize_field("PDataArray", z)?;
            ss.end()
        }
    }
    impl<'de> Deserialize<'de> for PCoordinates {
        fn deserialize<D>(d: D) -> Result<PCoordinates, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_struct("PCoordinates", &["PDataArray"; 3], PCoordinatesVisitor)
        }
    }
}

mod coordinates {
    use super::{Coordinates, DataArray};
    use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        DataArray,
    }

    struct CoordinatesVisitor;

    impl<'de> Visitor<'de> for CoordinatesVisitor {
        type Value = Coordinates;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("an array of 3 DataArrays")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let invalid_len_err = |n| <A::Error as serde::de::Error>::invalid_length(n, &self);
            let (_, x) = map
                .next_entry::<Field, DataArray>()?
                .ok_or_else(|| invalid_len_err(0))?;
            let (_, y) = map
                .next_entry::<Field, DataArray>()?
                .ok_or_else(|| invalid_len_err(1))?;
            let (_, z) = map
                .next_entry::<Field, DataArray>()?
                .ok_or_else(|| invalid_len_err(2))?;
            Ok(Coordinates([x, y, z]))
        }
    }

    impl Serialize for Coordinates {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use serde::ser::SerializeStruct;
            let Coordinates([x, y, z]) = self;
            let mut ss = s.serialize_struct("Coordinates", 3)?;
            ss.serialize_field("DataArray", x)?;
            ss.serialize_field("DataArray", y)?;
            ss.serialize_field("DataArray", z)?;
            ss.end()
        }
    }
    impl<'de> Deserialize<'de> for Coordinates {
        fn deserialize<D>(d: D) -> Result<Coordinates, D::Error>
        where
            D: Deserializer<'de>,
        {
            d.deserialize_struct("Coordinates", &["DataArray"; 3], CoordinatesVisitor)
        }
    }
}

mod data {
    use super::RawData;
    use serde::de::{Deserialize, Deserializer, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    /*
     * Data in an AppendedData element
     */

    struct RawDataVisitor;

    impl<'de> Visitor<'de> for RawDataVisitor {
        type Value = RawData;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("Raw byte data")
        }

        fn visit_bytes<E: serde::de::Error>(self, v: &[u8]) -> Result<Self::Value, E> {
            eprintln!("Deserializing as bytes");
            // Skip the first byte which always corresponds to the preceeding underscore
            if v.is_empty() {
                return Ok(RawData(Vec::new()));
            }
            if v[0] != b'_' {
                return Err(serde::de::Error::custom("Missing preceeding underscore"));
            }
            Ok(RawData(v[1..].to_vec()))
        }
    }

    impl Serialize for RawData {
        fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            if !self.0.is_empty() {
                let mut v = Vec::with_capacity(self.0.len() + 1);
                v.push(b'_');
                v.extend_from_slice(&self.0);
                s.serialize_bytes(&v)
            } else {
                s.serialize_bytes(&[])
            }
        }
    }

    impl<'de> Deserialize<'de> for RawData {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(d.deserialize_bytes(RawDataVisitor)?)
        }
    }
}

mod data_set {
    use super::*;
    use serde::ser::{Serialize, SerializeStruct, Serializer};

    impl Serialize for ImageData {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("ImageData", 3 + self.pieces.len())?;
            ss.serialize_field("WholeExtent", &self.whole_extent)?;
            ss.serialize_field("Origin", &vector3::Vector3(self.origin))?;
            ss.serialize_field("Spacing", &vector3::Vector3(self.spacing))?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for Grid {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("Grid", 1 + &self.pieces.len())?;
            ss.serialize_field("WholeExtent", &self.whole_extent)?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for Unstructured {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("Unstructured", self.pieces.len())?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for PImageData {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("PImageData", 6 + self.pieces.len())?;
            ss.serialize_field("GhostLevel", &self.ghost_level)?;
            ss.serialize_field("WholeExtent", &self.whole_extent)?;
            ss.serialize_field("Origin", &vector3::Vector3(self.origin))?;
            ss.serialize_field("Spacing", &vector3::Vector3(self.spacing))?;
            ss.serialize_field("PPointData", &self.point_data)?;
            ss.serialize_field("PCellData", &self.cell_data)?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for PUnstructured {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("PUnstructured", 4 + self.pieces.len())?;
            ss.serialize_field("GhostLevel", &self.ghost_level)?;
            ss.serialize_field("PPointData", &self.point_data)?;
            ss.serialize_field("PCellData", &self.cell_data)?;
            ss.serialize_field("PPoints", &self.points)?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for PRectilinearGrid {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("PRectilinearGrid", 5 + self.pieces.len())?;
            ss.serialize_field("GhostLevel", &self.ghost_level)?;
            ss.serialize_field("WholeExtent", &self.whole_extent)?;
            ss.serialize_field("PPointData", &self.point_data)?;
            ss.serialize_field("PCellData", &self.cell_data)?;
            ss.serialize_field("PCoordinates", &self.coords)?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
        }
    }

    impl Serialize for PStructuredGrid {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let mut ss = s.serialize_struct("PStructuredGrid", 5 + self.pieces.len())?;
            ss.serialize_field("GhostLevel", &self.ghost_level)?;
            ss.serialize_field("WholeExtent", &self.whole_extent)?;
            ss.serialize_field("PPointData", &self.point_data)?;
            ss.serialize_field("PCellData", &self.cell_data)?;
            ss.serialize_field("PPoints", &self.points)?;
            for p in &self.pieces {
                ss.serialize_field("Piece", p)?;
            }
            ss.end()
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
            let connectivity = connectivity.ok_or_else(|| make_err())?;
            let offsets = offsets.ok_or_else(|| make_err())?;
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
            Ok(d.deserialize_struct("Topo", &["DataArray"; 2], TopoVisitor)?)
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
            Ok(d.deserialize_struct("Cells", &["DataArray"; 3], CellsVisitor)?)
        }
    }
}

mod piece {
    use super::{AttributeData, Cells, Coordinates, Extent, Piece, Points, Topo};
    use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
    use std::fmt;

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        Extent,
        NumberOfPoints,
        NumberOfCells,
        NumberOfLines,
        NumberOfStrips,
        NumberOfPolys,
        NumberOfVerts,
        PointData,
        CellData,
        Polys,
        Points,
        Cells,
        Verts,
        Lines,
        Strips,
        Coordinates,
    }

    struct PieceVisitor;

    impl<'de> Visitor<'de> for PieceVisitor {
        type Value = Piece;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("piece data")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let mut piece = Piece {
                extent: None,
                number_of_points: 0,
                number_of_cells: 0,
                number_of_lines: 0,
                number_of_strips: 0,
                number_of_polys: 0,
                number_of_verts: 0,
                point_data: AttributeData::default(),
                cell_data: AttributeData::default(),
                polys: None,
                points: None,
                cells: None,
                verts: None,
                lines: None,
                strips: None,
                coordinates: None,
            };

            while let Some(key) = map.next_key::<Field>()? {
                match key {
                    Field::Extent => {
                        piece.extent = map.next_value::<Option<Extent>>()?;
                    }
                    Field::NumberOfPoints => {
                        piece.number_of_points = map.next_value::<u32>()?;
                    }
                    Field::NumberOfCells => {
                        piece.number_of_cells = map.next_value::<u32>()?;
                    }
                    Field::NumberOfLines => {
                        piece.number_of_lines = map.next_value::<u32>()?;
                    }
                    Field::NumberOfStrips => {
                        piece.number_of_strips = map.next_value::<u32>()?;
                    }
                    Field::NumberOfPolys => {
                        piece.number_of_polys = map.next_value::<u32>()?;
                    }
                    Field::NumberOfVerts => {
                        piece.number_of_verts = map.next_value::<u32>()?;
                    }
                    Field::PointData => {
                        piece.point_data = map.next_value::<AttributeData>()?;
                    }
                    Field::CellData => {
                        piece.cell_data = map.next_value::<AttributeData>()?;
                    }
                    Field::Polys => {
                        piece.polys = map.next_value::<Option<Topo>>()?;
                    }
                    Field::Points => {
                        piece.points = map.next_value::<Option<Points>>()?;
                    }
                    Field::Cells => {
                        piece.cells = map.next_value::<Option<Cells>>()?;
                    }
                    Field::Verts => {
                        piece.verts = map.next_value::<Option<Topo>>()?;
                    }
                    Field::Lines => {
                        piece.lines = map.next_value::<Option<Topo>>()?;
                    }
                    Field::Strips => {
                        piece.strips = map.next_value::<Option<Topo>>()?;
                    }
                    Field::Coordinates => {
                        piece.coordinates = map.next_value::<Option<Coordinates>>()?;
                    }
                }
            }

            Ok(piece)
        }
    }

    impl<'de> Deserialize<'de> for Piece {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(d.deserialize_struct(
                "Piece",
                &[
                    "Extent",
                    "NumberOfPoints",
                    "NumberOfCells",
                    "NumberOfLines",
                    "NumberOfStrips",
                    "NumberOfPolys",
                    "NumberOfVerts",
                    "PointData",
                    "CellData",
                    "Polys",
                    "Points",
                    "Cells",
                    "Verts",
                    "Lines",
                    "Strips",
                    "Coordinates",
                ],
                PieceVisitor,
            )?)
        }
    }
}

mod vtkfile {
    use super::{
        model, AppendedData, Compressor, DataSet, DataSetType, ScalarType, Unstructured, VTKFile,
    };
    use serde::de::{Deserialize, Deserializer, MapAccess, Visitor};
    use serde::ser::{Serialize, Serializer};
    use std::fmt;

    impl Serialize for VTKFile {
        fn serialize<S>(&self, s: S) -> std::result::Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            use serde::ser::SerializeStruct;
            let mut ss = s.serialize_struct("VTKFile", 7)?;
            ss.serialize_field("type", &self.data_set_type)?;
            ss.serialize_field("version", &self.version)?;
            ss.serialize_field("byte_order", &self.byte_order)?;
            ss.serialize_field("header_type", &self.header_type)?;
            ss.serialize_field("compressor", &self.compressor)?;
            ss.serialize_field("AppendedData", &self.appended_data)?;
            match &self.data_set {
                DataSet::ImageData(image_data) => ss.serialize_field("ImageData", image_data)?,
                DataSet::PolyData(unstructured) => ss.serialize_field("PolyData", unstructured)?,
                DataSet::RectilinearGrid(grid) => ss.serialize_field("RectilinearGrid", grid)?,
                DataSet::StructuredGrid(grid) => ss.serialize_field("StructuredGrid", grid)?,
                DataSet::UnstructuredGrid(grid) => ss.serialize_field("UnstructuredGrid", grid)?,
                DataSet::PImageData(image_data) => ss.serialize_field("PImageData", image_data)?,
                DataSet::PPolyData(unstructured) => {
                    ss.serialize_field("PPolyData", unstructured)?
                }
                DataSet::PRectilinearGrid(grid) => ss.serialize_field("PRectilinearGrid", grid)?,
                DataSet::PStructuredGrid(grid) => ss.serialize_field("PStructuredGrid", grid)?,
                DataSet::PUnstructuredGrid(grid) => {
                    ss.serialize_field("PUnstructuredGrid", grid)?
                }
            }

            ss.end()
        }
    }

    #[derive(Debug, serde::Deserialize)]
    #[serde(field_identifier)]
    enum Field {
        #[serde(rename = "type")]
        Type,
        #[serde(rename = "version")]
        Version,
        #[serde(rename = "byte_order")]
        ByteOrder,
        #[serde(rename = "header_type")]
        HeaderType,
        #[serde(rename = "compressor")]
        Compressor,
        AppendedData,
        ImageData,
        PolyData,
        RectilinearGrid,
        StructuredGrid,
        UnstructuredGrid,
        PImageData,
        PPolyData,
        PRectilinearGrid,
        PStructuredGrid,
        PUnstructuredGrid,
    }

    struct VTKFileVisitor;

    impl<'de> Visitor<'de> for VTKFileVisitor {
        type Value = VTKFile;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("vtk xml file data")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: MapAccess<'de>,
        {
            let mut vtk = VTKFile {
                data_set_type: DataSetType::UnstructuredGrid,
                version: model::Version::new((4, 1)),
                byte_order: model::ByteOrder::BigEndian,
                header_type: None,
                compressor: Compressor::None,
                appended_data: None,
                data_set: DataSet::UnstructuredGrid(Unstructured { pieces: Vec::new() }),
            };

            while let Some(key) = map.next_key::<Field>()? {
                match key {
                    Field::Type => {
                        vtk.data_set_type = map.next_value::<DataSetType>()?;
                    }
                    Field::Version => {
                        vtk.version = map.next_value::<model::Version>()?;
                    }
                    Field::ByteOrder => {
                        vtk.byte_order = map.next_value::<model::ByteOrder>()?;
                    }
                    Field::HeaderType => {
                        vtk.header_type = map.next_value::<Option<ScalarType>>()?;
                    }
                    Field::Compressor => {
                        vtk.compressor = map.next_value::<Compressor>()?;
                    }
                    Field::AppendedData => {
                        vtk.appended_data = map.next_value::<Option<AppendedData>>()?;
                    }
                    Field::ImageData
                    | Field::PolyData
                    | Field::RectilinearGrid
                    | Field::StructuredGrid
                    | Field::UnstructuredGrid
                    | Field::PImageData
                    | Field::PPolyData
                    | Field::PRectilinearGrid
                    | Field::PStructuredGrid
                    | Field::PUnstructuredGrid => {
                        vtk.data_set = map.next_value::<DataSet>()?;
                    }
                }
            }

            Ok(vtk)
        }
    }

    impl<'de> Deserialize<'de> for VTKFile {
        fn deserialize<D>(d: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            Ok(d.deserialize_struct(
                "VTKFile",
                &[
                    "type",
                    "version",
                    "byte_order",
                    "header_type",
                    "compressor",
                    "AppendedData",
                    "ImageData",
                    "PolyData",
                    "RectilinearGrid",
                    "StructuredGrid",
                    "UnstructuredGrid",
                    "PImageData",
                    "PPolyData",
                    "PRectilinearGrid",
                    "PStructuredGrid",
                    "PUnstructuredGrid",
                ],
                VTKFileVisitor,
            )?)
        }
    }
}

/*
 * The following defines the VTK XML model as Rust types, which is then serialized and deserialized
 * using serde.
 *
 * This model is exported in case users prefer to work with a bare bones VTK XML model without
 * additional handling of Legacy formats or on demand loading of "Parallel" XML files.
 */

#[derive(Clone, Debug, PartialEq)]
pub struct VTKFile {
    pub data_set_type: DataSetType,
    pub version: model::Version,
    pub byte_order: model::ByteOrder,
    pub header_type: Option<ScalarType>,
    pub compressor: Compressor,
    pub appended_data: Option<AppendedData>,
    pub data_set: DataSet,
}

impl Default for VTKFile {
    fn default() -> VTKFile {
        VTKFile {
            data_set_type: DataSetType::ImageData,
            version: model::Version::new((0, 0)),
            byte_order: model::ByteOrder::BigEndian,
            header_type: None,
            compressor: Compressor::None,
            appended_data: None,
            data_set: DataSet::UnstructuredGrid(Unstructured { pieces: Vec::new() }),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Compressor {
    LZ4,
    ZLib,
    LZMA,
    None,
}

impl Default for Compressor {
    fn default() -> Compressor {
        Compressor::None
    }
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
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

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct ImageData {
    #[serde(rename = "WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "Origin", deserialize_with = "vector3::deserialize")]
    origin: [f32; 3],
    #[serde(rename = "Spacing", deserialize_with = "vector3::deserialize")]
    spacing: [f32; 3],
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct Grid {
    #[serde(rename = "WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct Unstructured {
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct PImageData {
    #[serde(rename = "GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "Origin", deserialize_with = "vector3::deserialize")]
    origin: [f32; 3],
    #[serde(rename = "Spacing", deserialize_with = "vector3::deserialize")]
    spacing: [f32; 3],
    #[serde(rename = "PPointData")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct PRectilinearGrid {
    #[serde(rename = "GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "PPointData")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "PCoordinates")]
    coords: PCoordinates,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct PStructuredGrid {
    #[serde(rename = "GhostLevel")]
    ghost_level: u32,
    #[serde(rename = "WholeExtent")]
    whole_extent: Extent,
    #[serde(rename = "PPointData")]
    point_data: Option<PAttributeData>,
    #[serde(rename = "PCellData")]
    cell_data: Option<PAttributeData>,
    #[serde(rename = "PPoints")]
    points: PPoints,
    #[serde(rename = "Piece")]
    pieces: Vec<PieceSource>,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct PUnstructured {
    #[serde(rename = "GhostLevel")]
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
#[serde(rename_all = "PascalCase")]
pub struct PieceSource {
    extent: Option<Extent>,
    source: String,
}

/// Contents and attributes of the `PPointData` XML element.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct PAttributeData {
    #[serde(rename = "Scalars")]
    scalars: Option<String>,
    #[serde(rename = "Vectors")]
    vectors: Option<String>,
    #[serde(rename = "Normals")]
    normals: Option<String>,
    #[serde(rename = "Tensors")]
    tensors: Option<String>,
    #[serde(rename = "TCoords")]
    tcoords: Option<String>,
    #[serde(rename = "$value", default)]
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

#[derive(Clone, Debug, PartialEq)]
pub struct PCoordinates([PDataArray; 3]);

impl Default for PCoordinates {
    fn default() -> PCoordinates {
        let coord = PDataArray {
            scalar_type: ScalarType::Float32,
            name: String::new(),
            num_comp: 1,
        };
        PCoordinates([coord.clone(), coord.clone(), coord])
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
    UnstructuredGrid,
    PImageData,
    PPolyData,
    PRectilinearGrid,
    PStructuredGrid,
    PUnstructuredGrid,
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

#[derive(Clone, Debug, PartialEq, Default, Serialize)]
#[serde(rename_all = "PascalCase")]
pub struct Piece {
    pub extent: Option<Extent>,
    #[serde(default)]
    pub number_of_points: u32,
    #[serde(default, skip_serializing_if = "is_zero")]
    pub number_of_cells: u32,
    #[serde(default)]
    pub number_of_lines: u32,
    #[serde(default)]
    pub number_of_strips: u32,
    #[serde(default)]
    pub number_of_polys: u32,
    #[serde(default)]
    pub number_of_verts: u32,
    pub point_data: AttributeData,
    pub cell_data: AttributeData,
    pub points: Option<Points>,
    pub cells: Option<Cells>,
    pub verts: Option<Topo>,
    pub lines: Option<Topo>,
    pub strips: Option<Topo>,
    pub polys: Option<Topo>,
    pub coordinates: Option<Coordinates>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Points {
    #[serde(rename = "DataArray")]
    data: DataArray,
}

impl Points {
    pub fn from_io_buffer(buf: model::IOBuffer, bo: model::ByteOrder) -> Points {
        Points {
            data: DataArray::from_io_buffer(buf, bo).with_num_comp(3),
        }
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
    fn from_model_cells(cells: model::Cells, bo: model::ByteOrder) -> Cells {
        let model::Cells { cell_verts, types } = cells;
        let (connectivity, offsets) = cell_verts.into_xml();
        Cells {
            connectivity: DataArray::from_io_buffer(connectivity.into(), bo)
                .with_name("connectivity"),
            offsets: DataArray::from_io_buffer(offsets.into(), bo).with_name("offsets"),
            types: DataArray::from_io_buffer(
                types
                    .into_iter()
                    .map(|x| x as u8)
                    .collect::<model::IOBuffer>(),
                bo,
            )
            .with_name("types"),
        }
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this `Topo` struct into a `mode::VertexNumbers` type.
    pub fn into_model_cells(
        self,
        l: usize,
        appended: Option<&AppendedData>,
        bo: model::ByteOrder,
    ) -> std::result::Result<model::Cells, ValidationError> {
        use num_traits::FromPrimitive;

        let type_codes: Option<Vec<u8>> = self.types.into_io_buffer(l, appended, bo)?.into();
        let type_codes = type_codes.ok_or_else(|| ValidationError::InvalidDataFormat)?;
        let types: std::result::Result<Vec<model::CellType>, ValidationError> = type_codes
            .into_iter()
            .map(|x| model::CellType::from_u8(x).ok_or_else(|| ValidationError::InvalidCellType(x)))
            .collect();
        let types = types?;

        let offsets: Option<Vec<u64>> = self.offsets.into_io_buffer(l, appended, bo)?.cast_into();
        let offsets = offsets.ok_or_else(|| ValidationError::InvalidDataFormat)?;

        // Count the total number of vertices we expect in the connectivity array.
        let num_vertices: usize = offsets.last().map(|&x| x as usize).unwrap_or(0);

        let connectivity: Option<Vec<u64>> = self
            .connectivity
            .into_io_buffer(num_vertices, appended, bo)?
            .cast_into();
        let connectivity = connectivity.ok_or_else(|| ValidationError::InvalidDataFormat)?;
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
    fn from_model_topo(topo: model::VertexNumbers, bo: model::ByteOrder) -> Topo {
        let (connectivity, offsets) = topo.into_xml();
        Topo {
            connectivity: DataArray::from_io_buffer(connectivity.into(), bo)
                .with_name("connectivity"),
            offsets: DataArray::from_io_buffer(offsets.into(), bo).with_name("offsets"),
        }
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this `Topo` struct into a `mode::VertexNumbers` type.
    pub fn into_vertex_numbers(
        self,
        num_elements: usize,
        appended: Option<&AppendedData>,
        bo: model::ByteOrder,
    ) -> std::result::Result<model::VertexNumbers, ValidationError> {
        let offsets: Option<Vec<u64>> = self
            .offsets
            .into_io_buffer(num_elements, appended, bo)?
            .cast_into();
        let offsets = offsets.ok_or_else(|| ValidationError::InvalidDataFormat)?;

        // Get the number of elements in the connectivity array from the last offset.
        let num_values = usize::try_from(*offsets.last().unwrap_or(&0))
            .map_err(|_| ValidationError::MissingTopologyOffsets)?;
        let connectivity: Option<Vec<u64>> = self
            .connectivity
            .into_io_buffer(num_values, appended, bo)?
            .cast_into();
        Ok(model::VertexNumbers::XML {
            connectivity: connectivity.ok_or_else(|| ValidationError::InvalidDataFormat)?,
            offsets,
        })
    }
}

/// Attribute data corresponding to the `PointData` or `CellData` elements.
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct AttributeData {
    #[serde(rename = "Scalars")]
    pub scalars: Option<String>,
    #[serde(rename = "Vectors")]
    pub vectors: Option<String>,
    #[serde(rename = "Normals")]
    pub normals: Option<String>,
    #[serde(rename = "Tensors")]
    pub tensors: Option<String>,
    #[serde(rename = "TCoords")]
    pub tcoords: Option<String>,
    /// The (possibly empty) collection of data arrays representing individual attributes.
    #[serde(rename = "$value", default)]
    pub data_array: Vec<DataArray>,
}

#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct AttributeInfo {
    #[serde(rename = "Scalars")]
    pub scalars: Option<String>,
    #[serde(rename = "Vectors")]
    pub vectors: Option<String>,
    #[serde(rename = "Normals")]
    pub normals: Option<String>,
    #[serde(rename = "Tensors")]
    pub tensors: Option<String>,
    #[serde(rename = "TCoords")]
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
    pub fn from_model_attributes(
        attribs: Vec<model::Attribute>,
        byte_order: model::ByteOrder,
    ) -> Self {
        let mut attribute_data = AttributeData::default();
        for attrib in attribs {
            match attrib {
                model::Attribute::DataArray(data) => {
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
                        .push(DataArray::from_model_data_array(data, byte_order));
                }
                // Field attributes are not supported, they are simply ignored.
                _ => {}
            }
        }
        attribute_data
    }
    pub fn into_model_attributes(
        self,
        n: usize,
        appended_data: Option<&AppendedData>,
        bo: model::ByteOrder,
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
            .filter_map(|x| x.into_attribute(n, appended_data, &info, bo).ok())
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Coordinates([DataArray; 3]);

impl Coordinates {
    /// Construct `Coordinates` from `model::Coordinates`.
    pub fn from_model_coords(coords: model::Coordinates, bo: model::ByteOrder) -> Self {
        Coordinates([
            DataArray::from_io_buffer(coords.x, bo),
            DataArray::from_io_buffer(coords.y, bo),
            DataArray::from_io_buffer(coords.z, bo),
        ])
    }

    /// Given the expected number of elements and an optional appended data,
    /// converts this struct into a `mode::Coordinates` type.
    pub fn into_model_coordinates(
        self,
        [nx, ny, nz]: [usize; 3],
        appended: Option<&AppendedData>,
        bo: model::ByteOrder,
    ) -> std::result::Result<model::Coordinates, ValidationError> {
        let Coordinates([x, y, z]) = self;
        let x = x.into_io_buffer(nx, appended, bo)?;
        let y = y.into_io_buffer(ny, appended, bo)?;
        let z = z.into_io_buffer(nz, appended, bo)?;
        Ok(model::Coordinates { x, y, z })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PDataArray {
    #[serde(rename = "type")]
    pub scalar_type: ScalarType,
    #[serde(rename = "Name", default)]
    pub name: String,
    #[serde(rename = "NumberOfComponents", default = "default_num_comp")]
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct DataArray {
    #[serde(rename = "type")]
    pub scalar_type: ScalarType,
    #[serde(rename = "Name", default)]
    pub name: String,
    pub format: DataArrayFormat,
    pub offset: Option<u32>,
    #[serde(rename = "NumberOfComponents", default = "default_num_comp")]
    pub num_comp: u32,
    #[serde(rename = "RangeMin")]
    pub range_min: Option<f64>,
    #[serde(rename = "RangeMax")]
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
    pub fn from_model_data_array(data: model::DataArray, bo: model::ByteOrder) -> Self {
        let num_comp = u32::try_from(data.num_comp()).unwrap();
        DataArray {
            name: data.name,
            num_comp,
            ..DataArray::from_io_buffer(data.data, bo)
        }
    }
    /// Construct a binary `DataArray` from a given `model::FieldArray`.
    pub fn from_field_array(field: model::FieldArray, bo: model::ByteOrder) -> Self {
        DataArray {
            name: field.name,
            num_comp: field.elem,
            ..DataArray::from_io_buffer(field.data, bo)
        }
    }
    /// Construct a binary `DataArray` from a given [`model::IOBuffer`].
    pub fn from_io_buffer(buf: model::IOBuffer, bo: model::ByteOrder) -> Self {
        DataArray {
            scalar_type: buf.scalar_type().into(),
            data: vec![Data::Data(base64::encode(buf.into_bytes_with_size(bo)))],
            ..Default::default()
        }
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

    /// Convert this data array into a `model::FieldArray` type.
    ///
    /// The given arguments are the number of elements (not bytes) in the expected output
    /// buffer and an optional appended data reference.
    pub fn into_field_array(
        self,
        l: usize,
        appended: Option<&AppendedData>,
        byte_order: model::ByteOrder,
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

        let num_elements = usize::try_from(num_comp).unwrap() * l;

        let data = match format {
            DataArrayFormat::Appended => {
                if let Some(appended) = appended {
                    let mut start: usize = offset.unwrap_or(0).try_into().unwrap();
                    let num_bytes = num_elements * scalar_type.size();
                    let buf = match appended.encoding {
                        Encoding::Raw => {
                            // Skip the first 64 bits which gives the size of each component in bytes
                            start += 8;
                            let bytes = &appended.data.0[start..start + num_bytes];
                            IOBuffer::from_bytes(bytes, scalar_type.into(), byte_order)?
                        }
                        Encoding::Base64 => {
                            // Add one 64-bit integer that specifies the size of each component in bytes.
                            let num_target_bits = num_bytes * 8 + 64;
                            // Compute how many base64 chars we need to decode l elements.
                            let num_source_bytes =
                                num_target_bits / 6 + if num_target_bits % 6 == 0 { 0 } else { 1 };
                            let bytes = &appended.data.0[start..start + num_source_bytes];
                            let bytes = base64::decode(bytes)?;
                            // Skip the first 64 bits which gives the size of each component in bytes
                            IOBuffer::from_bytes(&bytes[8..], scalar_type.into(), byte_order)?
                        }
                    };
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
                // First byte gives the bytes
                let bytes = base64::decode(data[0].clone().into_string())?;
                let buf = IOBuffer::from_bytes(&bytes[8..], scalar_type.into(), byte_order)?;
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
                let string = data[0].clone().into_string();
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
        bo: model::ByteOrder,
    ) -> std::result::Result<model::DataArray, ValidationError> {
        // First convert into a field array.
        let model::FieldArray { name, data, elem } = self.into_field_array(l, appended, bo)?;

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
        bo: model::ByteOrder,
    ) -> std::result::Result<model::IOBuffer, ValidationError> {
        self.into_field_array(num_elements, appended, bo)
            .map(|model::FieldArray { data, .. }| data)
    }

    pub fn into_attribute(
        self,
        num_elements: usize,
        appended: Option<&AppendedData>,
        info: &AttributeInfo,
        bo: model::ByteOrder,
    ) -> std::result::Result<model::Attribute, ValidationError> {
        let data_array = self.into_model_data_array(num_elements, appended, info, bo)?;

        Ok(model::Attribute::DataArray(data_array))
    }
}

fn default_num_comp() -> u32 {
    1
}

/// The contents of a `DataArray` element.
///
/// Some VTK tools like ParaView may produce undocumented tags inside this
/// element. We capture and ignore those via the `Meta` variant. Otherwise this
/// is treated as a data string.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Data {
    Meta {
        #[serde(rename = "InformationKey", default)]
        info_key: (),
    },
    Data(String),
}

impl Data {
    fn into_string(self) -> String {
        match self {
            Data::Meta { .. } => String::new(),
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AppendedData {
    /// Encoding used in the `data` field.
    pub encoding: Encoding,
    /// Raw data in binary or base64 format.
    ///
    /// The underscore present in the XML files is added and removed during
    /// serialization and deserialization respectively.
    #[serde(rename = "$value")]
    pub data: RawData,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RawData(Vec<u8>);

impl Default for RawData {
    fn default() -> RawData {
        RawData(Vec::new())
    }
}

/// Supported binary encoding formats.
#[derive(Copy, Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Encoding {
    Base64,
    Raw,
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
    Model(model::Error),
    ParseFloat(std::num::ParseFloatError),
    ParseInt(std::num::ParseIntError),
    InvalidCellType(u8),
    TooManyElements(u32),
    MissingTopologyOffsets,
    MissingReferencedAppendedData,
    MissingCoordinates,
    DataArraySizeMismatch {
        name: String,
        expected: usize,
        actual: usize,
    },
    Base64Decode(base64::DecodeError),
    Deserialize(de::DeError),
    Unsupported,
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
            ValidationError::Model(source) => Some(source),
            ValidationError::Base64Decode(source) => Some(source),
            ValidationError::Deserialize(source) => Some(source),
            ValidationError::ParseFloat(source) => Some(source),
            ValidationError::ParseInt(source) => Some(source),
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
            ValidationError::Model(e) => write!(f, "Failed to convert model to xml: {}", e),
            ValidationError::ParseFloat(e) => write!(f, "Failed to parse a float: {}", e),
            ValidationError::ParseInt(e) => write!(f, "Failed to parse an int: {}", e),
            ValidationError::InvalidCellType(t) => write!(f, "Invalid cell type: {}", t),
            ValidationError::TooManyElements(n) => write!(f, "Too many elements: {}", n),
            ValidationError::MissingTopologyOffsets => write!(f, "Missing topology offsets"),
            ValidationError::MissingReferencedAppendedData => {
                write!(f, "Appended data is referenced but missing from the file")
            }
            ValidationError::MissingCoordinates => {
                write!(f, "Missing coordinates in rectilinear grid definition")
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
            ValidationError::Base64Decode(source) => write!(f, "Base64 decode error: {:?}", source),
            ValidationError::Deserialize(source) => {
                write!(f, "Failed to deserialize data: {:?}", source)
            }
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
            data_set_type,
            appended_data,
            data_set,
            ..
        } = xml;

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
                point: point_data.into_model_attributes(npts, appended_data, byte_order),
                cell: cell_data.into_model_attributes(ncells, appended_data, byte_order),
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
                                        byte_order,
                                    )
                                })
                                .transpose()?;
                            let lines = lines
                                .map(|lines| {
                                    lines.into_vertex_numbers(
                                        number_of_lines,
                                        appended_data,
                                        byte_order,
                                    )
                                })
                                .transpose()?;
                            let strips = strips
                                .map(|strips| {
                                    strips.into_vertex_numbers(
                                        number_of_strips,
                                        appended_data,
                                        byte_order,
                                    )
                                })
                                .transpose()?;
                            let polys = polys
                                .map(|polys| {
                                    polys.into_vertex_numbers(
                                        number_of_polys,
                                        appended_data,
                                        byte_order,
                                    )
                                })
                                .transpose()?;
                            Ok(model::Piece::Inline(Box::new(model::PolyDataPiece {
                                points: points.unwrap().data.into_io_buffer(
                                    number_of_points,
                                    appended_data,
                                    byte_order,
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
                            let coords =
                                coordinates.ok_or_else(|| ValidationError::MissingCoordinates)?;
                            let coords = coords.into_model_coordinates(
                                [nx as usize, ny as usize, nz as usize],
                                appended_data,
                                byte_order,
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
                                    byte_order,
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
                                            byte_order,
                                        )
                                    })
                                    .transpose()?
                                    .unwrap_or_default();
                                Ok(model::Piece::Inline(Box::new(
                                    model::UnstructuredGridPiece {
                                        points: points.unwrap().data.into_io_buffer(
                                            number_of_points,
                                            appended_data,
                                            byte_order,
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
                coords,
                pieces,
            }) => model::DataSet::RectilinearGrid {
                extent: whole_extent.into(),
                meta: Some(Box::new(model::MetaData::RectilinearGrid {
                    ghost_level,
                    coords: [
                        coords.0[0].scalar_type.into(),
                        coords.0[1].scalar_type.into(),
                        coords.0[2].scalar_type.into(),
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
        })
    }
}

impl TryFrom<model::Vtk> for VTKFile {
    type Error = Error;
    fn try_from(vtk: model::Vtk) -> Result<VTKFile> {
        let model::Vtk {
            version,
            byte_order,
            data: data_set,
            ..
        } = vtk;

        let appended_data = Vec::new();

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
                        let piece_data = piece.load_piece_data()?;
                        let model::ImageDataPiece { extent, data } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            point_data: AttributeData::from_model_attributes(
                                data.point, byte_order,
                            ),
                            cell_data: AttributeData::from_model_attributes(data.cell, byte_order),
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
                        let piece_data = piece.load_piece_data()?;
                        let model::StructuredGridPiece {
                            extent,
                            points,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            points: Some(Points::from_io_buffer(points, byte_order)),
                            point_data: AttributeData::from_model_attributes(
                                data.point, byte_order,
                            ),
                            cell_data: AttributeData::from_model_attributes(data.cell, byte_order),
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
                        let piece_data = piece.load_piece_data()?;
                        let model::RectilinearGridPiece {
                            extent,
                            coords,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            extent: Some(extent.into()),
                            coordinates: Some(Coordinates::from_model_coords(coords, byte_order)),
                            point_data: AttributeData::from_model_attributes(
                                data.point, byte_order,
                            ),
                            cell_data: AttributeData::from_model_attributes(data.cell, byte_order),
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
                        let piece_data = piece.load_piece_data()?;
                        let num_points = piece_data.num_points();
                        let model::UnstructuredGridPiece {
                            points,
                            cells,
                            data,
                        } = piece_data;
                        Ok(Piece {
                            number_of_points: u32::try_from(num_points).unwrap(),
                            number_of_cells: u32::try_from(cells.num_cells()).unwrap(),
                            points: Some(Points::from_io_buffer(points, byte_order)),
                            cells: Some(Cells::from_model_cells(cells, byte_order)),
                            point_data: AttributeData::from_model_attributes(
                                data.point, byte_order,
                            ),
                            cell_data: AttributeData::from_model_attributes(data.cell, byte_order),
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
                        let piece_data = piece.load_piece_data()?;
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

                        let verts = verts.map(|topo| Topo::from_model_topo(topo, byte_order));
                        let lines = lines.map(|topo| Topo::from_model_topo(topo, byte_order));
                        let polys = polys.map(|topo| Topo::from_model_topo(topo, byte_order));
                        let strips = strips.map(|topo| Topo::from_model_topo(topo, byte_order));

                        Ok(Piece {
                            number_of_points: u32::try_from(num_points).unwrap(),
                            number_of_lines: u32::try_from(number_of_lines).unwrap(),
                            number_of_verts: u32::try_from(number_of_verts).unwrap(),
                            number_of_polys: u32::try_from(number_of_polys).unwrap(),
                            number_of_strips: u32::try_from(number_of_strips).unwrap(),
                            points: Some(Points::from_io_buffer(points, byte_order)),
                            verts,
                            lines,
                            polys,
                            strips,
                            point_data: AttributeData::from_model_attributes(
                                data.point, byte_order,
                            ),
                            cell_data: AttributeData::from_model_attributes(data.cell, byte_order),
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
                        .map(|data| Piece {
                            extent: Some(Extent([0, data.len() as i32, 0, 0, 0, 0])),
                            cell_data: AttributeData {
                                data_array: vec![DataArray::from_field_array(data, byte_order)],
                                ..Default::default()
                            },
                            ..Default::default()
                        })
                        .collect(),
                })
            }
        };

        let data_set_type = DataSetType::from(&data_set);
        let appended_data = if appended_data.is_empty() {
            None
        } else {
            Some(AppendedData {
                encoding: Encoding::Raw,
                data: RawData(appended_data),
            })
        };

        Ok(VTKFile {
            data_set_type,
            version,
            byte_order,
            header_type: None,
            compressor: Compressor::None,
            appended_data,
            data_set,
        })
    }
}

/// Import an XML VTK file from the specified path.
pub(crate) fn import(file_path: impl AsRef<Path>) -> Result<VTKFile> {
    let f = std::fs::File::open(file_path)?;
    parse(std::io::BufReader::new(f))
}

/// Parse an XML VTK file from the given reader.
pub(crate) fn parse(reader: impl BufRead) -> Result<VTKFile> {
    Ok(de::from_reader(reader)?)
}

/// Import an XML VTK file from the specified path.
#[cfg(feature = "async_blocked")]
pub(crate) async fn import_async(file_path: impl AsRef<Path>) -> Result<VTKFile> {
    let f = tokio::fs::File::open(file_path).await?;
    // Blocked on async support from quick-xml (e.g. https://github.com/tafia/quick-xml/pull/233)
    Ok(de::from_reader(std::io::BufReader::new(f))?)
}

/// Export an XML VTK file to the specified path.
pub(crate) fn export(vtk: &VTKFile, file_path: impl AsRef<Path>) -> Result<()> {
    let f = std::fs::File::create(file_path)?;
    write(vtk, std::io::BufWriter::new(f))
}

/// Write an XML VTK file to the specified writer.
pub(crate) fn write(vtk: &VTKFile, writer: impl Write) -> Result<()> {
    Ok(se::to_writer(writer, &vtk)?)
}

impl std::fmt::Display for VTKFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", se::to_string(self).map_err(|_| std::fmt::Error)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Verify that xmls with default empty meshes files work
    #[test]
    fn empty_image_data() {
        let image_data = r#" 
        <VTKFile type="ImageData" version="4.2" byte_order="BigEndian">
            <ImageData WholeExtent="0 1 0 1 0 1" Origin="0.0 0.0 0.0" Spacing="0.1 0.1 0.1">
                <Piece Extent="0 1 0 1 0 1">
                    <PointData Scalars="Temperature" Vectors="Velocity"></PointData>
                    <CellData Tensors="Stress"></CellData>
                </Piece>
            </ImageData>
        </VTKFile>"#;

        let vtk: VTKFile = de::from_str(image_data).unwrap();
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{:?}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
    }

    #[test]
    fn image_data() {
        let image_data = r#" 
        <VTKFile type="ImageData" version="4.2" byte_order="BigEndian">
        <ImageData WholeExtent="0 1 0 1 0 1" Origin="0 0 0" Spacing="0.1 0.1 0.1">
        <Piece Extent="0 1 0 1 0 1">
          <PointData Scalars="Temperature" Vectors="Velocity">
            <DataArray Name="Temperature" type="Float32" format="ascii">0 1 2 3 4 5 6 7 8 9</DataArray>
            <DataArray Name="Velocity" type="Float32" NumberOfComponents="3" format="ascii">
              0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
            </DataArray>
          </PointData>
          <CellData></CellData>
        </Piece>
        </ImageData>
        </VTKFile>"#;

        let vtk: VTKFile = de::from_str(image_data).unwrap();
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
    }

    #[test]
    fn polys() -> Result<()> {
        let polys = r#"<Polys>
          <DataArray type="Int32" Name="connectivity" format="ascii">
             0 1 2 3 4 5 6 7 0 1 5 4 2 3 7 6 0 4 7 3 1 2 6 5
          </DataArray>
          <DataArray type="Int32" Name="offsets" format="ascii">
             4 8 12 16 20 24
          </DataArray>
        </Polys>"#;

        let vtk: Topo = de::from_str(polys)?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk)?;
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str)?;
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

        let vtk: Points = de::from_str(points)?;
        let as_str = se::to_string(&vtk)?;
        let vtk_roundtrip = de::from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn piece() -> Result<()> {
        let piece = r#"
        <Piece NumberOfPoints="8" NumberOfVerts="0" NumberOfLines="0"
               NumberOfStrips="0" NumberOfPolys="6">
        <Points>
          <DataArray type="Float32" NumberOfComponents="3" format="ascii">
            0 0 0 1 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 1 1 0 1 1
          </DataArray>
        </Points>
        <PointData Scalars="my_scalars">
          <DataArray type="Float32" Name="my_scalars" format="ascii">
            0 1 2 3 4 5 6 7
         </DataArray>
        </PointData>
        <CellData Scalars="cell_scalars" Normals="cell_normals">
          <DataArray type="Int32" Name="cell_scalars" format="ascii">
           0 1 2 3 4 5
          </DataArray>
          <DataArray type="Float32" Name="cell_normals"
                     NumberOfComponents="3" format="ascii">
            0 0 -1 0 0 1 0 -1 0 0 1 0 -1 0 0 1 0 0
          </DataArray>
        </CellData>
        <Polys>
          <DataArray type="Int32" Name="connectivity" format="ascii">
             0 1 2 3 4 5 6 7 0 1 5 4 2 3 7 6 0 4 7 3 1 2 6 5
          </DataArray>
          <DataArray type="Int32" Name="offsets" format="ascii">
             4 8 12 16 20 24
          </DataArray>
        </Polys>
        </Piece>"#;

        let vtk: Piece = de::from_str(piece)?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk)?;
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str)?;
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_ascii() -> Result<()> {
        let vtk = import("assets/RectilinearGrid_ascii.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_compressed() -> Result<()> {
        let vtk = import("assets/RectilinearGridCompressed.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_appended_base64() -> Result<()> {
        let vtk = import("assets/RectilinearGridAppendedBase64.vtr")?;
        eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        eprintln!("{:?}", &as_bytes);
        let vtk_roundtrip = de::from_reader(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_appended_raw_binary() -> Result<()> {
        let vtk = import("assets/RectilinearGridRawBinary.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        eprintln!("{:?}", &as_bytes);
        let vtk_roundtrip = de::from_reader(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn rectilinear_grid_inline_binary() -> Result<()> {
        let vtk = import("assets/RectilinearGridInlineBinary.vtr")?;
        //eprintln!("{:#?}", &vtk);
        let as_bytes = se::to_bytes(&vtk)?;
        eprintln!("{:?}", &as_bytes);
        let vtk_roundtrip = de::from_reader(as_bytes.as_slice()).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn single_point() -> Result<()> {
        let inline_raw = import("assets/point.vtp")?;
        let vtk_inline_raw: model::Vtk = inline_raw.try_into()?;
        let vtk_ascii: model::Vtk = import("assets/point_ascii.vtp")?.try_into()?;
        assert_eq!(&vtk_inline_raw, &vtk_ascii);
        Ok(())
    }

    // Ensure that all binary formats produce identical (lossless) instances.
    #[test]
    fn rectilinear_grid_binary_all() -> Result<()> {
        let inline_raw = import("assets/RectilinearGridInlineBinary.vtr")?;
        let vtk_inline_raw: model::Vtk = inline_raw.try_into()?;
        let vtk_app_raw: model::Vtk = import("assets/RectilinearGridRawBinary.vtr")?.try_into()?;
        let vtk_app_base64: model::Vtk =
            import("assets/RectilinearGridAppendedBase64.vtr")?.try_into()?;
        //let vtk = import("assets/RectilinearGridCompressed.vtr")?;
        //let vtk = import("assets/RectilinearGrid.pvtr")?;
        assert_eq!(&vtk_inline_raw, &vtk_app_raw);
        assert_eq!(&vtk_inline_raw, &vtk_app_base64);
        Ok(())
    }

    #[test]
    fn parallel_rectilinear_grid() -> Result<()> {
        let vtk = import("assets/RectilinearGrid.pvtr")?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn poly_cube() -> Result<()> {
        let vtk = import("assets/polyEx0.vtp")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn parallel_cube() -> Result<()> {
        let vtk = import("assets/cube.pvtp")?;
        //eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        //eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn unstructured_cube() -> Result<()> {
        let vtk = import("assets/cube.vtu")?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn unstructured_cube_ascii() -> Result<()> {
        let vtk = import("assets/cube_ascii.vtu")?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn unstructured_cube_inline_binary() -> Result<()> {
        let vtk = import("assets/cube_inline_binary.vtu")?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        let vtk_ascii = import("assets/cube_ascii.vtu")?;
        eprintln!("{:#?}", &vtk_ascii);
        // Verify that the ascii cube is the same as the inline binary cube.
        let vtk_ascii = model::Vtk::try_from(vtk_ascii.clone())?;
        let vtk_binary = model::Vtk::try_from(vtk.clone())?;
        assert_eq!(&vtk_ascii, &vtk_binary);
        Ok(())
    }

    #[test]
    fn parallel_compressed_cube() -> Result<()> {
        let vtk = import("assets/cube_compressed.pvtu")?;
        eprintln!("{:#?}", &vtk);
        let as_str = se::to_string(&vtk).unwrap();
        eprintln!("{}", &as_str);
        let vtk_roundtrip = de::from_str(&as_str).unwrap();
        assert_eq!(vtk, vtk_roundtrip);
        Ok(())
    }

    #[test]
    fn coordinates() -> Result<()> {
        let xml = r#"<Coordinates>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            <DataArray type="Float32" format="ascii">1 2 3</DataArray>
            </Coordinates>"#;
        let coords: Coordinates = de::from_str(xml)?;
        let xml_round_trip = se::to_string(&coords)?;
        let round_trip: Coordinates = de::from_str(&xml_round_trip)?;
        assert_eq!(round_trip, coords);
        Ok(())
    }

    #[test]
    fn data_array_appended() -> Result<()> {
        let appended_xml = "<DataArray type=\"Float32\" Name=\"vectors\" \
                            NumberOfComponents=\"3\" format=\"appended\" offset=\"0\"/>";
        let appended: DataArray = de::from_str(appended_xml)?;
        let appended_xml_round_trip = se::to_string(&appended)?;
        let appended_round_trip: DataArray = de::from_str(&appended_xml_round_trip)?;
        assert_eq!(appended_round_trip, appended);
        Ok(())
    }

    #[test]
    fn data_array_binary() -> Result<()> {
        let binary_xml = "<DataArray type=\"Float32\" Name=\"scalars\" format=\"binary\"> \
                          _bAAAAAAAAAAAAIA/AAAAQAAAQEAAAIBA </DataArray>";

        let binary: DataArray = de::from_str(binary_xml)?;
        //eprintln!("bin:{:?}", binary.data);
        let binary_xml_round_trip = se::to_string(&binary)?;
        let binary_round_trip: DataArray = de::from_str(&binary_xml_round_trip)?;
        assert_eq!(binary_round_trip, binary);
        Ok(())
    }

    #[test]
    fn data_array_ascii() -> Result<()> {
        let ascii_xml =
            "<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\"> 10 20 30 </DataArray>";

        let ascii: DataArray = de::from_str(ascii_xml)?;
        let ascii_xml_round_trip = se::to_string(&ascii)?;
        let ascii_round_trip: DataArray = de::from_str(&ascii_xml_round_trip)?;
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

        let arr: DataArray = de::from_str(xml)?;
        let xml_round_trip = se::to_string(&arr)?;
        let round_trip: DataArray = de::from_str(&xml_round_trip)?;
        assert_eq!(round_trip, arr);
        Ok(())
    }

    /*
     * Test conversion facilities
     */

    #[test]
    fn xml_to_vtk_conversion() -> Result<()> {
        use model::*;
        let xml = import("assets/RectilinearGrid_ascii.vtr")?;
        let vtk: Vtk = xml.clone().try_into()?;
        assert_eq!(
            vtk,
            Vtk {
                version: Version::new((1, 0)),
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
                })
            }
        );
        Ok(())
    }

    #[test]
    fn vtk_xml_conversion_round_trip() -> Result<()> {
        use model::*;
        let xml = import("assets/RectilinearGrid_ascii.vtr")?;
        let vtk: Vtk = xml.clone().try_into()?;
        let xml_round_trip: VTKFile = vtk.clone().try_into()?;
        let vtk_round_trip: Vtk = xml_round_trip.clone().try_into()?;
        assert_eq!(vtk.clone(), vtk_round_trip,);
        assert_eq!(xml_round_trip.clone(), vtk_round_trip.try_into()?);
        Ok(())
    }
}
