//!
//! Internal APIs for dealing with XML VTK file types.
//!
//! See [VTK XML Format
//! Reference](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/#xml-file-formats) for
//! details on the xml format.
//!

//use std::io::BufRead;
//use std::collections::HashMap;
use std::path::Path;

use quick_xml::{
    //events::{BytesStart, Event},
    de,
    //Reader,
};
use serde::Deserialize;

use crate::model;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    XML(quick_xml::Error),
    IO(std::io::Error),
    Deserialization(de::DeError),
    InvalidVersion,
    TypeExtensionMismatch,
    InvalidType,
    InvalidByteOrder,
    MissingAttribute(AttribName),
    InvalidAttributeValueFor(AttribName),
    UnexpectedElement(String),
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::XML(source) => write!(f, "XML error: {:?}", source),
            Error::IO(source) => write!(f, "I/O error: {:?}", source),
            Error::Deserialization(source) => write!(f, "Deserialization error: {:?}", source),
            Error::InvalidVersion => write!(f, "VTK version must be in \"major.minor\" format"),
            Error::InvalidByteOrder => write!(
                f,
                "Byte order must be one of \"BigEndian\" or \"LittleEndian\""
            ),
            Error::InvalidType => write!(f, "Invalid VTKFile type detected"),
            Error::InvalidAttributeValueFor(attrib) => {
                write!(f, "Invalid attribute value for {}", attrib)
            }
            Error::MissingAttribute(attrib) => write!(f, "Missing attribute: {}", attrib),
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
            _ => None,
        }
    }
}

impl From<quick_xml::Error> for Error {
    fn from(e: quick_xml::Error) -> Error {
        Error::XML(e)
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

#[derive(Debug, Deserialize, PartialEq)]
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

#[derive(Debug, Deserialize, PartialEq)]
pub struct VTKFile {
    #[serde(rename = "type")]
    _type: DataSetType,
    #[serde(rename = "ImageData")]
    image_data: Option<ImageData>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct ImageData {
    #[serde(rename = "WholeExtent")]
    whole_extent: [u32; 6],
    #[serde(rename = "Origin")]
    origin: [f32; 3],
    #[serde(rename = "Spacing")]
    spacing: [f32; 3],
    #[serde(rename = "Piece")]
    pieces: Vec<Piece>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Piece {
    #[serde(rename = "Extent")]
    extent: [u32; 6],
    point_data: Option<PointData>,
    cell_data: Option<CellData>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct PointData {
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
    #[serde(rename = "DataArray")]
    data_array: Vec<DataArray>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct CellData {
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
    #[serde(rename = "DataArray")]
    data_array: Vec<DataArray>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Coordinates {
    #[serde(rename = "DataArray")]
    data_array: Vec<DataArray>,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct DataArray {
    #[serde(rename = "type")]
    _type: ScalarType,
    #[serde(rename = "Name")]
    name: String,
    format: DataArrayFormat,
    offset: u32,
}

#[derive(Debug, Deserialize, PartialEq)]
pub enum ScalarType {
    Float32,
    Float64,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum DataArrayFormat {
    Appended,
}

/// An enumeration of all possible attributes expected inside a VTK XML tag.
#[derive(Copy, Clone, Debug)]
pub enum AttribName {
    Spacing,
    Origin,
    WholeExtent,
    Extent,

    Scalars,
    Vectors,
    Normals,
    Tensors,
    TCoords,

    Name,
    Unknown,
}

impl AttribName {
    pub fn as_str(&self) -> &'static str {
        match self {
            AttribName::Spacing => "Spacing",
            AttribName::Origin => "Origin",
            AttribName::WholeExtent => "WholeExtent",
            AttribName::Extent => "Extent",

            AttribName::Scalars => "Scalars",
            AttribName::Vectors => "Vectors",
            AttribName::Normals => "Normals",
            AttribName::Tensors => "Tensors",
            AttribName::TCoords => "TCoords",

            AttribName::Name => "Name",
            AttribName::Unknown => "Unknown",
        }
    }

    pub fn from_byte_str(bytes: &[u8]) -> Self {
        match bytes {
            b"Spacing" => AttribName::Spacing,
            b"Origin" => AttribName::Origin,
            b"WholeExtent" => AttribName::WholeExtent,
            b"Extent" => AttribName::Extent,

            b"Scalars" => AttribName::Scalars,
            b"Vectors" => AttribName::Vectors,
            b"Normals" => AttribName::Normals,
            b"Tensors" => AttribName::Tensors,
            b"TCoords" => AttribName::TCoords,

            b"Name" => AttribName::Name,
            _ => AttribName::Unknown,
        }
    }

    pub fn as_byte_str(&self) -> &'static [u8] {
        self.as_str().as_bytes()
    }
}

impl std::fmt::Display for AttribName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
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

    pub fn try_from_byte_str_serial(ty: &[u8]) -> Option<FileType> {
        DataType::try_from_byte_str(ty).map(|data| FileType {
            storage: StorageFormat::Serial,
            data,
        })
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ByteOrder {
    BigEndian,
    LittleEndian,
}

/*
fn parse_element<'a, B: BufRead, Tag, Data>(
    reader: &mut Reader<B>,
    buf: &mut Vec<u8>,
    mut process_tag: impl FnMut(BytesStart) -> Result<Tag>,
    mut process_inner: impl FnMut(&mut Reader<B>, &mut Vec<u8>, Tag) -> Result<Data>,
) -> Result<Data> {
    let mut out = None;
    let mut name = None;
    loop {
        match reader.read_event(buf) {
            Ok(Event::Start(e)) => {
                eprintln!("{:?}", String::from_utf8_lossy(e.name()));
                if name.is_none() {
                    name = Some(e.name().to_owned());
                    let result = process_tag(e)?;
                    out = Some(process_inner(reader, buf, result)?);
                }
            }
            Ok(Event::End(event)) => {
                eprintln!("{:?}", event);
                if let Some(name) = name.as_ref() {
                    if event.name() == name.as_slice() {
                        break;
                    }
                }
            }
            Err(err) => Err(err)?,
            Ok(Event::Eof) => break,
            _ => {}
        }
    }

    Ok(out.unwrap())
}

fn parse_extent_attrib(value: &[u8]) -> Result<model::Extent> {
    use crate::parser::u32_b;
    let res = ws!(value, count_fixed!(u32, u32_b, 6));
    let ex = res.to_full_result().map_err(|_| {
        Error::InvalidAttributeValueFor(AttribName::WholeExtent)
    })?;
    Ok(model::Extent::Ranges([
            ex[0]..=ex[1],
            ex[2]..=ex[3],
            ex[4]..=ex[5],
    ]))
}

/// Parse points element
///
/// # XML Example
///
/// ```xml
/// <Points>
///     <DataArray NumberOfComponents=”3” .../>
/// </Points>
/// ```
fn parse_points() -> Result<()> {
    Ok(())
}

/// Parse coordinates
///
/// # XML Example
///
/// ```xml
/// <Coordinates>
///     <DataArray .../>
///     <DataArray .../>
///     <DataArray .../>
/// </Coordinates>
/// ```
fn parse_coords() -> Result<()> {
    Ok(())
}

/// Parse vertices
///
/// # XML Example
///
/// ```xml
/// <Verts>
///     <DataArray type=”Int32” Name=”connectivity” .../>
///     <DataArray type=”Int32” Name=”offsets” .../>
/// </Verts>
/// ```
fn parse_verts() -> Result<()> {
    Ok(())
}

/// Parse cells
///
/// # XML Example
///
/// ```xml
/// <Cells>
///     <DataArray type=”Int32” Name=”connectivity” .../>
///     <DataArray type=”Int32” Name=”offsets” .../>
///     <DataArray type=”UInt8” Name=”types” .../>
/// </Cells>
/// ```
fn parse_cells() -> Result<()> {
    Ok(())
}

fn parse_image_data_attribs(event: BytesStart) -> Result<(model::Extent, [f32; 3], [f32; 3])> {
    use crate::parser::{f32_b, u32_b};
    let mut extent = None;
    let mut origin = None;
    let mut spacing = None;
    for attr in event.attributes() {
        let attr = attr?;
        match AttribName::from_byte_str(attr.key) {
            AttribName::WholeExtent => {
                extent = Some(parse_extent_attrib(&*attr.value)?);
            }
            AttribName::Origin => {
                let parsed =
                    ws!(&*attr.value, count_fixed!(f32, f32_b, 3)).to_full_result();
                origin = Some(parsed.map_err(|_| {
                    Error::InvalidAttributeValueFor(AttribName::Origin)
                })?);
            }
            AttribName::Spacing => {
                let parsed =
                    ws!(&*attr.value, count_fixed!(f32, f32_b, 3)).to_full_result();
                spacing = Some(parsed.map_err(|_| {
                    Error::InvalidAttributeValueFor(AttribName::Spacing)
                })?);
            }
            _ => {}
        }
    }
    Ok((
        extent.ok_or(Error::MissingAttribute(AttribName::WholeExtent))?,
        origin.ok_or(Error::MissingAttribute(AttribName::Origin))?,
        spacing.ok_or(Error::MissingAttribute(AttribName::Spacing))?,
    ))
}

/// Parse a `DataArray` element.
///
/// # XML Examples
///
/// ```xml
/// <DataArray type=”Float32” Name=”vectors” NumberOfComponents=”3”
///            format=”appended” offset=”0”/>
///
/// <DataArray type=”Float32” Name=”scalars” format=”binary”>
///            bAAAAAAAAAAAAIA/AAAAQAAAQEAAAIBA... </DataArray>
///
/// <DataArray type=”Int32” Name=”offsets” format=”ascii”>
///            10 20 30 ... </DataArray>
/// ```
///
fn parse_data_array<B: BufRead>(reader: &mut Reader<B>, buf: &mut Vec<u8>) -> Result<(String, model::Attribute)> {
    parse_element(
        reader,
        buf,
        |event| {
            let tag_name = event.name();
            if tag_name != b"DataArray" {
                return Err(Error::UnexpectedElement(tag_name.to_string()));
            }
            let mut attrib_name = String::new();
            for attr in event.attributes() {
                let attr = attr?;
                match AttribName::from_byte_str(attr.key) {
                    AttribName::Name => {
                        // Clear attrib_name to prevent multiple name attributes from accumulating.
                        attrib_name.clear();
                        attrib_name.push_str(String::from_utf8_lossy(&*attr.value));
                    }
                    _ => {} // Silently ignore invalid attributes
                }
            }
            attrib_name
        },
        |reader, buf, attrib_name| {
            Ok((attrib_name, model::Attribute::Vectors { data: vec![].into() }))
        }
    );
}

/// An intermediate helper enum for parsing the `Piece` element contents.
enum PieceElem {
    PointData(Vec<model::DataArray>),
    CellData(Vec<model::DataArray),
    Points(model::DataArray),
    Cells(model::Cells),
    PolyData(model::PolyDataTopology),
}

/// Parse point or cell data.
///
/// # XML Examples
///
/// ```xml
/// <PointData Scalars=”Temperature” Vectors=”Velocity”>
///     <DataArray Name=”Velocity” .../>
///     <DataArray Name=”Temperature” .../>
///     <DataArray Name=”Pressure” .../>
/// </PointData>
/// ```
fn parse_piece_elem<B: BufRead>(reader: &mut Reader<B>, buf: &mut Vec<u8>) -> Result<PieceElem> {
    parse_element(
        reader,
        buf,
        |event| {
            match event.name() {
                b"PointData" => {}
                b"CellData" => {}
                b"Points" => {}
                b"Cells" => {}
                b"PolyData" => {}
                return Err(Error::UnexpectedElement(event.name().to_string()));
            }
            let mut attribs = HashMap::new();
            for attr in event.attributes() {
                let attr = attr?;
                attribs.insert(AttribName::from_byte_str(attr.key), &*attr.value);
            }
            attribs
        },
        |reader, buf, attribs| {
            let point = parse_data_array(reader, buf)?;
            Ok(model::Attributes {
                point,
                cell,
            })
        }
    );
}

fn parse_piece<B: BufRead>(
    reader: &mut Reader<B>,
    buf: &mut Vec<u8>,
) -> Result<model::Piece> {
    parse_element(
        reader,
        buf,
        |event| {
            let tag_name = event.name();
            if tag_name != b"Piece" {
                return Err(Error::UnexpectedElement(tag_name.to_string()));
            }
            let mut extent = None;
            for attr in event.attributes() {
                let attr = attr?;
                match AttribName::from_byte_str(attr.key) {
                    AttribName::Extent => {
                        extent = Some(parse_extent_attrib(&*attr.value)?);
                    }
                    _ => {} // Silently ignore invalid attributes
                }
            }
            extent
        },
        |reader, buf, extent| {
            for 
            let piece_elems = parse_piece_elem(reader, buf)?;
            Ok(model::Piece::Inline(Box::new(model::PieceData::ImageData {
                extent,
                data
            })))
        }
    );
}

fn parse_data_set<B: BufRead>(
    reader: &mut Reader<B>,
    buf: &mut Vec<u8>,
) -> Result<model::DataSet> {
    parse_element(
        reader,
        buf,
        |event| {
            let tag_name = event.name();
            let file_type = DataType::try_from_byte_str(tag_name).expect("Unknown data type");
            Ok(match file_type {
                DataType::ImageData => parse_image_data_attribs(event)?,
                _ => Err(Error::InvalidType)?,
            })
        },
        |reader, buf, (extent, origin, spacing)| {
            use model::*;
            let piece = parse_piece(reader, buf)?;
            Ok(DataSet::ImageData {
                extent,
                origin,
                spacing,
                pieces: vec![piece]
            })
        },
    )
}


fn parse<B: BufRead>(
    mut reader: Reader<B>,
    mut xml_file_type: Option<FileType>,
) -> Result<model::Vtk> {
    use model::*;

    let mut buf = Vec::new();

    // Read the VTKFile tag
    let mut version = None;
    let mut data = None;
    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Start(ref e)) => {
                let mut byte_order = None;
                eprintln!("{:?}", String::from_utf8_lossy(e.name()));

                match e.name() {
                    b"VTKFile" => {
                        for attr in e.attributes() {
                            let attr = attr?;
                            match attr.key {
                                b"type" => {
                                    // Attribute must match the type we get from the extension if any
                                    if let Some(xml_file_type) = xml_file_type {
                                        xml_file_type.data.validate(&attr.value)?;
                                    } else {
                                        xml_file_type = Some(
                                            FileType::try_from_byte_str_serial(&attr.value)
                                                .ok_or(Error::InvalidType)?,
                                        );
                                    }
                                }
                                b"version" => {
                                    use crate::parser::u8_b;
                                    let ver = separated_pair!(&*attr.value, u8_b, tag!("."), u8_b);
                                    match ver {
                                        nom::IResult::Done(_, o) => version = Some(Version::new(o)),
                                        _ => return Err(Error::InvalidVersion),
                                    }
                                }
                                b"byte_order" => {
                                    byte_order = Some(match &*attr.value {
                                        b"BigEndian" => ByteOrder::BigEndian,
                                        b"LittleEndian" => ByteOrder::LittleEndian,
                                        _ => return Err(Error::InvalidByteOrder),
                                    });
                                }
                                b"compressor" => {
                                    // TODO: Look up vtkDataCompressor
                                }
                                _ => {} // Ignore unknown attributes
                            }
                        }
                    }
                    _ => {} // Ignore unknown tags
                }

                let byte_order = byte_order.ok_or(Error::Unknown)?;
                data = Some(parse_data_set(&mut reader, &mut buf)?);
            }
            Ok(Event::End(ref event)) => {
                if event.name() == b"VTKFile" {
                    break;
                }
            }
            Err(err) => Err(err)?,
            Ok(Event::Eof) => break,
            _ => {}
        }
    }

    Ok(Vtk {
        version: version.unwrap(),
        title: String::new(),
        data: data.unwrap(),
    })
}
*/

/// Helper function for importing serial XML VTK files.
pub(crate) fn import(file_path: &Path, xml_file_type: FileType) -> Result<model::Vtk> {
    let f = std::fs::File::open(file_path)?;
    let vtk_file = de::from_reader(std::io::BufReader::new(f))?;
    Ok(model::Vtk {
        version: (1, 0).into(),
        title: String::new(),
        data: model::DataSet::PolyData { pieces: vec![] }
    })
    //let mut reader = Reader::from_file(file_path)?;
    //reader.trim_text(true);
    //parse(reader, Some(xml_file_type))
}

/// Helper function for parsing serial XML VTK strings.
pub(crate) fn parse_str(xml: &str) -> Result<VTKFile> {
    Ok(de::from_str(xml)?)
    //let mut reader = Reader::from_str(xml);
    //reader.trim_text(true);
    //parse(reader, None)
}

#[cfg(test)]
mod tests {
    use super::*;

    /*
     * Verify that xmls with default empty meshes files work
     */
    #[test]
    fn empty_image_data() {
        let image_data = r#" 
        <VTKFile type="ImageData" version="4.2" byte_order="BigEndian">
            <ImageData WholeExtent="0 1 0 1 0 1" Origin="0.0 0.0 0.0" Spacing="0.1 0.1 0.1">
                <Piece Extent="0 1 0 1 0 1">
                    <PointData></PointData>
                    <CellData></CellData>
                </Piece>
            </ImageData>
        </VTKFile>"#;

        let vtk = parse_str(image_data);
        eprintln!("{:?}", vtk);
    }
}
