#[macro_use]
extern crate nom;

#[macro_use]
pub mod basic;

pub mod model;
pub mod parser;
pub mod writer;
pub mod xml;

use std::fs::File;
use std::io;
use std::path::Path;

/// Error type for Import/Export operations.
#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Write(writer::Error),
    Parse(nom::ErrorKind<u32>),
    XML(xml::Error),
    UnknownFileExtension(Option<String>),
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::IO(source) => write!(f, "IO error: {:?}", source),
            Error::Write(source) => write!(f, "Write error: {:?}", source),
            Error::Parse(source) => write!(f, "Parse error: {:?}", source),
            Error::XML(source) => write!(f, "XML error: {:?}", source),
            Error::UnknownFileExtension(Some(ext)) => {
                write!(f, "Unknown file extension: {:?}", ext)
            }
            Error::UnknownFileExtension(None) => write!(f, "Missing file extension"),
            Error::Unknown => write!(f, "Unknown error"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IO(source) => Some(source),
            Error::Write(_) => None, // TODO: implement std::error for writer::Error
            Error::Parse(_) => None,
            Error::XML(source) => Some(source),
            Error::UnknownFileExtension(_) => None,
            Error::Unknown => None,
        }
    }
}

/// Convert `std::io` error into `vtkio` error.
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IO(e)
    }
}

/// Convert [`xml::Error`] error into the top level `vtkio` error.
///
/// [`xml::Error`]: xml.enum.Error.html
impl From<xml::Error> for Error {
    fn from(e: xml::Error) -> Error {
        Error::XML(e)
    }
}

/// Convert `vtkio` error into `std::io` error.
impl From<Error> for io::Error {
    fn from(err: Error) -> io::Error {
        match err {
            Error::IO(e) => e,
            _ => io::Error::new(io::ErrorKind::Other, format!("{:?}", err)),
        }
    }
}

impl From<writer::Error> for Error {
    fn from(e: writer::Error) -> Error {
        Error::Write(e)
    }
}

/// Helper function that implements the actual importing routine for legacy VTK files.
fn import_vtk<F>(file_path: &Path, parse: F) -> Result<model::Vtk, Error>
where
    F: Fn(&[u8]) -> nom::IResult<&[u8], model::Vtk>,
{
    use crate::io::Read;
    use nom::IResult;

    let mut file = File::open(file_path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    match parse(&buf) {
        IResult::Done(_, vtk) => Ok(vtk),
        IResult::Error(e) => Err(Error::Parse(e.into_error_kind())),
        IResult::Incomplete(_) => Err(Error::Unknown),
    }
}

/// Import a VTK file at the specified path.
///
/// This function determines the vtk file type from the extension as prescribed by the [VTK
/// file formats documentation](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf):
///
///  - Legacy (`.vtk`) -- Simple legacy file format (Non-XML)
///  - Image data (`.vti`) -- Serial vtkImageData (structured)
///  - PolyData (`.vtp`) -- Serial vtkPolyData (unstructured)
///  - RectilinearGrid (`.vtr`) -- Serial vtkRectilinearGrid (structured)
///  - StructuredGrid (`.vts`) -- Serial vtkStructuredGrid (structured)
///  - UnstructuredGrid (`.vtu`) -- Serial vtkUnstructuredGrid (unstructured)
///  - PImageData (`.pvti`) -- Parallel vtkImageData (structured)
///  - PPolyData (`.pvtp`) -- Parallel vtkPolyData (unstructured)
///  - PRectilinearGrid (`.pvtr`) -- Parallel vtkRectilinearGrid (structured)
///  - PStructuredGrid (`.pvts`) -- Parallel vtkStructuredGrid (structured)
///  - PUnstructuredGrid (`.pvtu`) -- Parallel vtkUnstructuredGrid (unstructured)
///
/// # Examples
///
/// The following example imports a legacy `.vtk` file called `tet.vtk`, and panics with an
/// appropriate error message if the file fails to load.
///
/// ```should_panic
/// use vtkio::{model, import, export_ascii};
/// use std::path::PathBuf;
///
/// let file_path = PathBuf::from("tet.vtk");
///
/// let mut vtk_file = import(&file_path)
///     .expect(&format!("Failed to load file: {:?}", file_path));
/// ```
pub fn import(file_path: impl AsRef<Path>) -> Result<model::Vtk, Error> {
    let path = file_path.as_ref();
    let ext = path
        .extension()
        .and_then(|s| s.to_str())
        .ok_or(Error::UnknownFileExtension(None))?;
    match ext {
        "vtk" => import_vtk(path, parser::parse_be),
        ext => {
            let ft = xml::FileType::try_from_ext(ext)
                .ok_or(Error::UnknownFileExtension(Some(ext.to_string())))?;
            let vtk_file = xml::import(path)?;
            let exp_ft = xml::FileType::from(vtk_file.data_set_type);
            if ft != exp_ft {
                Err(Error::XML(xml::Error::TypeExtensionMismatch))
            } else {
                use std::convert::TryInto;
                Ok(vtk_file.try_into()?)
            }
        }
    }
}

/// Import an XML VTK file in raw form.
///
/// This importer performs a direct translation from the XML string to a Rust representation
/// without any decoding or conversion.
///
/// [`VTKFile`] is used internally as an intermediate step for constructing the [`Vtk`] model,
/// which has built-in facilities for loading pieces referenced in "parallel" XML formats as well
/// as representing Legacy VTK formats, which are more compact when serialized.
///
/// [`Vtk`]: model/struct.Vtk.html
/// [`VTKFile`]: xml/struct.VTKFile.html
#[cfg(feature = "unstable")]
pub fn import_xml(file_path: impl AsRef<Path>) -> Result<xml::VTKFile, Error> {
    let path = file_path.as_ref();
    let ext = path
        .extension()
        .and_then(|s| s.to_str())
        .ok_or(Error::UnknownFileExtension(None))?;

    // Check that the file extension is one of the known ones.
    let _ = xml::FileType::try_from_ext(ext)
        .ok_or(Error::UnknownFileExtension(Some(ext.to_string())))?;

    Ok(xml::import(path)?)
}

/// Import a legacy VTK file at the specified path.
///
/// If the file is in binary, numeric types will be interpreted in little-endian format.
/// For the default byte order used by most `.vtk` files use [`import`] or [`import_be`].
///
/// [`import`]: fn.import.html
/// [`import_be`]: fn.import_be.html
pub fn import_le(file_path: impl AsRef<Path>) -> Result<model::Vtk, Error> {
    import_vtk(file_path.as_ref(), parser::parse_le)
}

/// Import a legacy VTK file at the specified path.
///
/// If the file is in binary, numeric types will be interpreted in big-endian format.
/// This function behaves the same as [`import`], but expects the given file to be strictly in
/// legacy `.vtk` format.
///
/// [`import`]: fn.import.html
pub fn import_be(file_path: impl AsRef<Path>) -> Result<model::Vtk, Error> {
    import_vtk(file_path.as_ref(), parser::parse_be)
}

/// Export given vtk data to the specified file in BINARY format.
///
/// Endianness is determined by the `byte_order` field of the [`Vtk`] type.
///
/// # Examples
///
/// ```no_run
/// use vtkio::model::*;
/// use vtkio::export;
/// use std::path::PathBuf;
/// let vtk = Vtk {
///     version: Version::new((4,1)),
///     byte_order: ByteOrder::BigEndian,
///     title: String::from("Tetrahedron"),
///     data: DataSet::inline(PieceData::UnstructuredGrid {
///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0].into(),
///         cells: Cells {
///             cell_verts: VertexNumbers::Legacy {
///                 num_cells: 1,
///                 vertices: vec![4, 0, 1, 2, 3]
///             },
///             types: vec![CellType::Tetra],
///         },
///         data: Attributes::new(),
///     })
/// };
/// export(vtk, "test.vtk");
/// ```
///
/// [`Vtk`]: struct.Vtk.html
pub fn export(data: model::Vtk, file_path: impl AsRef<Path>) -> Result<(), Error> {
    use crate::io::Write;
    use crate::writer::WriteVtk;

    let mut file = File::create(file_path.as_ref())?;
    file.write_all(Vec::<u8>::new().write_vtk(data)?.as_slice())?;
    Ok(())
}

/// Export given vtk data to the specified file in ASCII format.
///
/// # Examples
///
/// ```no_run
/// use vtkio::model::*;
/// use vtkio::export_ascii;
/// use std::path::PathBuf;
/// let vtk = Vtk {
///     version: Version::new((4,1)),
///     title: String::from("Tetrahedron"),
///     byte_order: ByteOrder::BigEndian,
///     data: DataSet::inline(PieceData::UnstructuredGrid {
///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0].into(),
///         cells: Cells {
///             cell_verts: VertexNumbers::Legacy {
///                 num_cells: 1,
///                 vertices: vec![4, 0, 1, 2, 3]
///             },
///             types: vec![CellType::Tetra],
///         },
///         data: Attributes::new(),
///     })
/// };
/// export_ascii(vtk, "test.vtk");
/// ```
pub fn export_ascii(data: model::Vtk, file_path: impl AsRef<Path>) -> Result<(), Error> {
    use crate::io::Write;
    use crate::writer::WriteVtk;

    let mut out_str = String::new();
    out_str.write_vtk(data)?;
    {
        let mut file = File::create(file_path.as_ref())?;
        file.write_all(out_str.as_bytes())?;
    }
    Ok(())
}
