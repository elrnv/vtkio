//!
//! Internal APIs for dealing with XML VTK file types.
//!

use crate::model;
use quick_xml::{Reader, events::Event};
use std::path::Path;

#[derive(Debug)]
pub enum Error {
    XML(quick_xml::Error),
    InvalidVersion,
    TypeExtensionMismatch,
    InvalidByteOrder,
}


impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::XML(source) => write!(f, "XML error: {:?}", source),
            Error::InvalidVersion => write!(f, "VTK version must be in \"major.minor\" format"),
            Error::InvalidByteOrder => write!(f, "Byte order must be one of \"BigEndian\" or \"LittleEndian\""),
            Error::TypeExtensionMismatch => write!(f, "The extension of the VTK file doesn't match the type specified in the VTKFile tag"),
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ByteOrder {
    BigEndian,
    LittleEndian,
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
}

fn parse_data_set(e: &quick_xml::events::BytesText) -> Result<model::DataSet, Error> {
    use model::*;
    Ok(DataSet::UnstructuredGrid {
        points: Vec::<f64>::new().into(),
        cells: Cells {
            num_cells: 0,
            vertices: vec![],
        },
        cell_types: vec![],
        data: Attributes {
            point: vec![],
            cell: vec![],
        }
    })
}

/// Helper function for importing serial XML VTK files.
pub(crate) fn import(file_path: &Path, xml_file_type: FileType) -> Result<model::Vtk, Error> {
    use model::*;

    let mut reader = Reader::from_file(file_path)?;
    reader.trim_text(true);

    let mut buf = Vec::new();

    // Read the VTKFile tag
    let mut version = None;
    let mut byte_order = None;
    let mut data = None;
    match reader.read_event(&mut buf) {
        Ok(Event::Start(ref e)) => {
            match e.name() {
                b"VTKFile" => {
                    for attr in e.attributes() {
                        let attr = attr?;
                        match attr.key {
                            b"type" => {
                                // This attribute must match the type we get from the extension.
                                xml_file_type.data.validate(&attr.value)?;
                            }
                            b"version" => {
                                use crate::parser::u8_b;
                                let ver = separated_pair!(u8_b, tag!("."), u8_b);
                                match ver {
                                    nom::IResult::Done(_, o) => version = Some(Version::new(o)),
                                    _ => return Err(Error::InvalidVersion),
                                }
                            }
                            b"byte_order" => {
                                byte_order = Some(match attr.value {
                                    b"BigEndian" => ByteOrder::BigEndian,
                                    b"LittleEndian" => ByteOrder::LittleEndian,
                                    _ => return Err(Error::InvalidByteOrder),
                                });
                            }
                            b"compressor" => {
                                // TODO: Look up vtkDataCompressor
                            }
                        }
                    }
                }
            }
        }
        Ok(Event::Text(ref event)) => {
            let byte_order = byte_order.ok_or(Error::Unknown)?;
            parse_data_set(event)
        }
        Err(err) => {
        }
        Ok(Event::Eof) => {
        }
    }

    Ok(Vtk {
        version: version.unwrap(),
        title: "Hello".to_string(),
        data: data.unwrap(),
    })
}
