//!
//! Internal APIs for dealing with XML VTK file types.
//!

use std::io::BufRead;
use std::path::Path;

use quick_xml::{events::Event, Reader};

use crate::model;

#[derive(Debug)]
pub enum Error {
    XML(quick_xml::Error),
    InvalidVersion,
    TypeExtensionMismatch,
    InvalidType,
    InvalidByteOrder,
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::XML(source) => write!(f, "XML error: {:?}", source),
            Error::InvalidVersion => write!(f, "VTK version must be in \"major.minor\" format"),
            Error::InvalidByteOrder => write!(
                f,
                "Byte order must be one of \"BigEndian\" or \"LittleEndian\""
            ),
            Error::InvalidType => write!(f, "Invalid VTKFile type detected"),
            Error::TypeExtensionMismatch => write!(
                f,
                "The extension of the VTK file doesn't match the type specified in the VTKFile tag"
            ),
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
            ext => return None,
        })
    }

    pub fn try_from_byte_str_serial(ty: &[u8]) -> Option<FileType> {
        DataType::try_from_byte_str(ty).map(|data| FileType {
                storage: StorageFormat::Serial,
                data,
            },
        )
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
    fn validate(&self, fmt: &[u8]) -> Result<(), Error> {
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

    fn try_from_byte_str(ty: &[u8]) -> Option<DataType> {
        Some(match ty {
            b"ImageData" => DataType::ImageData,
            b"PolyData" => DataType::PolyData,
            b"RectilinearGrid" => DataType::RectilinearGrid,
            b"StructuredGrid" => DataType::StructuredGrid,
            b"Unstructured" => DataType::UnstructuredGrid,
            ty => return None,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ByteOrder {
    BigEndian,
    LittleEndian,
}

fn parse_tag<B: BufRead, Out>(
    reader: &mut Reader<B>,
    buf: &mut Vec<u8>,
    mut inner: impl FnMut(&[u8], &mut Reader<B>, &mut Vec<u8>) -> Result<Out, Error>,
) -> Result<Out, Error> {
    let mut out = None;
    let mut name = None;
    loop {
        match reader.read_event(buf) {
            Ok(Event::Start(ref e)) => {
                eprintln!("{:?}", String::from_utf8_lossy(e.name()));
                if name.is_none() {
                    name = Some(e.name().to_owned());
                    out = Some(inner(name.as_ref().unwrap(), reader, buf)?);
                }
            }
            Ok(Event::End(ref event)) => {
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

fn parse_data_set<B: BufRead>(reader: &mut Reader<B>, buf: &mut Vec<u8>) -> Result<model::DataSet, Error> {

    parse_tag(
        reader,
        buf,
        |tag_name, reader, buf| {
            let file_type = DataType::try_from_byte_str(tag_name).expect("Unknown data type");
            Ok(match file_type {
                DataType::ImageData => {
                    use model::*;
                    DataSet::ImageData {
                        points: Vec::<f64>::new().into(),
                        cells: Cells {
                            num_cells: 0,
                            vertices: vec![],
                        },
                        cell_types: vec![],
                        data: Attributes {
                            point: vec![],
                            cell: vec![],
                        },
                    }
                }
                _ => {panic!("Unknown data type")}
            })
        }
    )
}

fn parse<B: BufRead>(
    mut reader: Reader<B>,
    mut xml_file_type: Option<FileType>,
) -> Result<model::Vtk, Error> {
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

/// Helper function for importing serial XML VTK files.
pub(crate) fn import(file_path: &Path, xml_file_type: FileType) -> Result<model::Vtk, Error> {
    let mut reader = Reader::from_file(file_path)?;
    reader.trim_text(true);
    parse(reader, Some(xml_file_type))
}

/// Helper function for parsing serial XML VTK strings.
pub(crate) fn parse_str(xml: &str) -> Result<model::Vtk, Error> {
    let mut reader = Reader::from_str(xml);
    reader.trim_text(true);
    parse(reader, None)
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
            <ImageData WholeExtent="0.0 1.0 0.0 1.0 0.0 1.0" Origin="0.0 0.0 0.0" Spacing="0.1 0.1 0.1">
                <Piece Extent="0.0 0.5 0.0 0.5 0.0 0.5">
                    <PointData></PointData>
                    <CellData></CellData>
                </Piece>
            </ImageData>
        </VTKFile>"#;

        let vtk = parse_str(image_data);
        eprintln!("{:?}", vtk);
    }
}
