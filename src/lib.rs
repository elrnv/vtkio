//! Import and export library for Visualization Toolkit (VTK)
//! [files](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/).
//!
//! Legacy `.vtk` files as well as modern XML formats are supported.
//! Both "serial" and "parallel" XML files are supported with facilities for lazily loading.
//!
//! The [`Vtk`] struct exposes the primary IO API.
//!
//! # Examples
//!
//! Many sample files can be found in the `assets` directory.
//!
//! For the following example, we will load a VTK file named `tet.vtk`, modify it and write it back
//! in Legacy ASCII format.
//!
//! ```no_run
//! use vtkio::model::*; // import model definition of a VTK file
//! fn main() {
//!     use std::path::PathBuf;
//!     let file_path = PathBuf::from("../assets/tet.vtk");
//!
//!     let mut vtk_file = Vtk::import(&file_path)
//!         .expect(&format!("Failed to load file: {:?}", file_path));
//!
//!     vtk_file.version = Version::new((4,2)); // arbitrary change
//!
//!     vtk_file.export_ascii(&file_path)
//!         .expect(&format!("Failed to save file: {:?}", file_path));
//! }
//! ```
//!
//! Files are sometimes provided as strings or byte slices, so it is also useful to be able to
//! parse VTK files and write them back to a string or byte slice.
//!
//! ```no_run
//! use vtkio::model::*; // import model definition of a VTK file
//! fn main() {
//!     let data: &[u8] = include_str!("../assets/tet.vtk").as_bytes(); // Or just include_bytes!
//!
//!     let mut vtk_file = Vtk::parse_legacy_be(data).expect(&format!("Failed to parse file"));
//!
//!     vtk_file.version = Version::new((4,2)); // arbitrary change
//!
//!     let mut output = String::new();
//!     Vtk::write_legacy_ascii(vtk_file, &mut output).expect(&format!("Failed to write file"));
//!
//!     println!("{}", output);
//! }
//! ```
//!
//! To quickly extract some data from a file, you can cast it to an `f64` type as follows
//!
//! ```no_run
//! use vtkio::model::*; // import model definition of a VTK file
//!
//! // Load up vtk file.
//! let file_path = "../assets/para_tet.vtk";
//! let mut vtk = Vtk::import(&file_path)
//!     .expect(&format!("Failed to load file: {:?}", file_path));
//!
//! // Get all the pieces knowing that type they are.
//! let pieces = if let DataSet::UnstructuredGrid { pieces, .. } = vtk.data {
//!     pieces
//! } else {
//!     panic!("Wrong vtk data type");
//! };
//!
//! // Often files have only a single piece, so we load it up here.
//! // To avoid cloning you can also use `into_loaded_piece_data` here.
//! let piece = pieces[0].load_piece_data(None).unwrap();
//!
//! // Get the first cell attribute.
//! let attribute = &piece.data.cell[0];
//!
//! // Find the attribute with a specific name (in this case "FloatValue"),
//! // and return the corresponding data in `f64` format.
//! let field_name = "FloatValue";
//! let data = if let Attribute::Field { data_array, .. } = attribute {
//!     data_array
//!         .iter()
//!         .find(|&DataArrayBase { name, .. }| name == field_name)
//!         .expect(&format!("Failed to find {:?} field", field_name))
//!         .data
//!         .cast_into::<f64>() // Cast to f64 to get a view of the data.
//!         .expect("Failed cast to f64")
//! } else {
//!     panic!("First attribute is not a field");
//! };
//!
//! assert_eq!(data.as_slice(), &[0.0]);
//! ```
#[macro_use]
extern crate nom;

#[doc = include_str!("examples/vertex.rs")]
mod examples;

#[macro_use]
pub mod basic;

#[macro_use]
pub mod model;
pub mod parser;
pub mod writer;
#[cfg(feature = "xml")]
pub mod xml;

#[cfg(feature = "xml")]
use std::convert::{TryFrom, TryInto};
use std::fs::File;
#[cfg(feature = "xml")]
use std::io::BufRead;
use std::io::{self, BufWriter, Read, Write};
use std::path::Path;

use crate::writer::{AsciiWriter, BinaryWriter, WriteVtk};

pub use model::IOBuffer;

/// The primary `vtkio` API is provided through the `Vtk` struct.
pub use model::Vtk;

/// Error type for Import/Export operations.
#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Write(writer::Error),
    Parse(nom::ErrorKind<u32>),
    #[cfg(feature = "xml")]
    XML(xml::Error),
    UnknownFileExtension(Option<String>),
    Load(model::Error),
    Unknown,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::IO(source) => write!(f, "IO error: {}", source),
            Error::Write(source) => write!(f, "Write error: {}", source),
            Error::Parse(source) => write!(f, "Parse error: {:?}", source),
            #[cfg(feature = "xml")]
            Error::XML(source) => write!(f, "XML error: {}", source),
            Error::UnknownFileExtension(Some(ext)) => {
                write!(f, "Unknown file extension: {:?}", ext)
            }
            Error::UnknownFileExtension(None) => write!(f, "Missing file extension"),
            Error::Load(source) => write!(f, "Load error: {}", source),
            Error::Unknown => write!(f, "Unknown error"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IO(source) => Some(source),
            Error::Write(source) => Some(source),
            Error::Parse(_) => None,
            #[cfg(feature = "xml")]
            Error::XML(source) => Some(source),
            Error::UnknownFileExtension(_) => None,
            Error::Load(source) => Some(source),
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

/// Convert `vtkio::model::Error` into a `vtkio::Error`.
impl From<model::Error> for Error {
    fn from(e: model::Error) -> Error {
        Error::Load(e)
    }
}

/// Convert [`xml::Error`] error into the top level `vtkio` error.
///
/// [`xml::Error`]: xml.enum.Error.html
#[cfg(feature = "xml")]
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

impl Vtk {
    /// Helper for parsing legacy VTK files.
    fn parse_vtk<F>(mut reader: impl Read, parse: F, buf: &mut Vec<u8>) -> Result<Vtk, Error>
    where
        F: Fn(&[u8]) -> nom::IResult<&[u8], Vtk>,
    {
        use nom::IResult;
        reader.read_to_end(buf)?;
        match parse(buf) {
            IResult::Done(_, vtk) => Ok(vtk),
            IResult::Error(e) => Err(Error::Parse(e.into_error_kind())),
            IResult::Incomplete(_) => Err(Error::Unknown),
        }
    }

    /// Helper for importing legacy VTK files from the given path.
    fn import_vtk<F>(file_path: &Path, parse: F) -> Result<Vtk, Error>
    where
        F: Fn(&[u8]) -> nom::IResult<&[u8], Vtk>,
    {
        let file = File::open(file_path)?;
        Vtk::parse_vtk(file, parse, &mut Vec::new())
    }

    /// Parse a legacy VTK file from the given reader.
    ///
    /// If the file is in binary format, numeric types will be interpreted in big endian format,
    /// which is the most common among VTK files.
    /// Note that this function and [`parse_legacy_le`](Vtk::parse_legacy_le) also work equally well for
    /// parsing VTK files in ASCII format.
    ///
    /// # Examples
    ///
    /// Parsing an ASCII file:
    ///
    /// ```
    /// use vtkio::model::*; // import the model definition of a VTK file
    /// let vtk_ascii: &[u8] = b"
    /// ## vtk DataFile Version 2.0
    /// Triangle example
    /// ASCII
    /// DATASET POLYDATA
    /// POINTS 3 float
    /// 0.0 0.0 0.0
    /// 1.0 0.0 0.0
    /// 0.0 0.0 -1.0
    ///
    /// POLYGONS 1 4
    /// 3 0 1 2
    /// ";
    ///
    /// let vtk = Vtk::parse_legacy_be(vtk_ascii).expect("Failed to parse vtk file");
    ///
    /// assert_eq!(vtk, Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::BigEndian,
    ///     title: String::from("Triangle example"),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::Legacy {
    ///             num_cells: 1,
    ///             vertices: vec![3, 0, 1, 2]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// });
    /// ```
    pub fn parse_legacy_be(reader: impl Read) -> Result<Vtk, Error> {
        Vtk::parse_vtk(reader, parser::parse_be, &mut Vec::new())
    }

    /// Parse a legacy VTK file from the given reader.
    ///
    /// If the file is in binary format, numeric types will be interpreted in little endian format.
    /// Note that this function and [`parse_legacy_be`](Vtk::parse_legacy_be) also work equally well for
    /// parsing VTK files in ASCII format.
    ///
    /// # Examples
    ///
    /// Parsing an ASCII file:
    ///
    /// ```
    /// use vtkio::model::*; // import the model definition of a VTK file
    /// let vtk_ascii: &[u8] = b"
    /// ## vtk DataFile Version 2.0
    /// Triangle example
    /// ASCII
    /// DATASET POLYDATA
    /// POINTS 3 float
    /// 0.0 0.0 0.0
    /// 1.0 0.0 0.0
    /// 0.0 0.0 -1.0
    ///
    /// POLYGONS 1 4
    /// 3 0 1 2
    /// ";
    ///
    /// let vtk = Vtk::parse_legacy_le(vtk_ascii).expect("Failed to parse vtk file");
    ///
    /// assert_eq!(vtk, Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::LittleEndian,
    ///     title: String::from("Triangle example"),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::Legacy {
    ///             num_cells: 1,
    ///             vertices: vec![3, 0, 1, 2]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// });
    /// ```
    pub fn parse_legacy_le(reader: impl Read) -> Result<Vtk, Error> {
        Vtk::parse_vtk(reader, parser::parse_le, &mut Vec::new())
    }

    /// Parse a legacy VTK file in big endian format from the given reader and a buffer.
    ///
    /// This is the buffered version of [`Vtk::parse_legacy_be`](Vtk::parse_legacy_be), which allows one to reuse the same
    /// heap allocated space when reading many files.
    pub fn parse_legacy_buf_be(reader: impl Read, buf: &mut Vec<u8>) -> Result<Vtk, Error> {
        Vtk::parse_vtk(reader, parser::parse_be, buf)
    }

    /// Parse a legacy VTK file in little endian format from the given reader and a buffer.
    ///
    /// This is the buffered version of [`parse_legacy_le`](Vtk::parse_legacy_le), which allows one to reuse the same
    /// heap allocated space when reading many files.
    pub fn parse_legacy_buf_le(reader: impl Read, buf: &mut Vec<u8>) -> Result<Vtk, Error> {
        Vtk::parse_vtk(reader, parser::parse_le, buf)
    }

    /// Parse a modern XML style VTK file from a given reader.
    ///
    /// # Examples
    ///
    /// Parsing a binary file in big endian format representing a polygon mesh consisting of a single
    /// triangle:
    ///
    /// ```
    /// use vtkio::model::*; // import the model definition of a VTK file
    ///
    /// let input: &[u8] = b"\
    /// <VTKFile type=\"PolyData\" version=\"2.0\" byte_order=\"BigEndian\", header_type=\"UInt64\">\
    ///   <PolyData>\
    ///     <Piece NumberOfPoints=\"3\" NumberOfLines=\"0\" NumberOfStrips=\"0\" NumberOfPolys=\"1\" NumberOfVerts=\"0\">\
    ///       <PointData/>\
    ///       <CellData/>\
    ///       <Points>\
    ///         <DataArray type=\"Float32\" format=\"binary\" NumberOfComponents=\"3\">\
    ///           AAAAAAAAAAQAAAAAAAAAAAAAAAA/gAAAAAAAAAAAAAAAAAAAAAAAAL+AAAA=\
    ///         </DataArray>\
    ///       </Points>\
    ///       <Polys>\
    ///         <DataArray type=\"UInt64\" Name=\"connectivity\" format=\"binary\" NumberOfComponents=\"1\">\
    ///           AAAAAAAAAAgAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAI=\
    ///         </DataArray>\
    ///         <DataArray type=\"UInt64\" Name=\"offsets\" format=\"binary\" NumberOfComponents=\"1\">\
    ///           AAAAAAAAAAgAAAAAAAAAAw==\
    ///         </DataArray>\
    ///       </Polys>\
    ///     </Piece>\
    ///   </PolyData>\
    /// </VTKFile>";
    ///
    /// let vtk = Vtk::parse_xml(input).expect("Failed to parse XML VTK file");
    ///
    /// assert_eq!(vtk, Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::BigEndian, // This is default
    ///     title: String::new(),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::XML {
    ///             connectivity: vec![0, 1, 2],
    ///             offsets: vec![3]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// });
    /// ```
    #[cfg(feature = "xml")]
    pub fn parse_xml(reader: impl BufRead) -> Result<Vtk, Error> {
        // There is no extension to check with the data is provided directly.
        // Luckily the xml file contains all the data necessary to determine which data is
        // being parsed.
        let vtk_file = xml::parse(reader)?;
        Ok(vtk_file.try_into()?)
    }

    #[cfg(feature = "async_blocked")]
    async fn import_vtk_async<F>(file_path: &Path, parse: F) -> Result<Vtk, Error>
    where
        F: Fn(&[u8]) -> nom::IResult<&[u8], Vtk>,
    {
        use nom::IResult;
        use tokio::fs::File;
        use tokio::io::AsyncReadExt;

        let mut file = File::open(file_path).await?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).await?;
        match parse(&buf) {
            IResult::Done(_, vtk) => Ok(vtk),
            IResult::Error(e) => Err(Error::Parse(e.into_error_kind())),
            IResult::Incomplete(_) => Err(Error::Unknown),
        }
    }

    /// Import a VTK file at the specified path.
    ///
    /// This function determines the vtk file type from the extension as prescribed by the [VTK
    /// file formats documentation](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/):
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
    /// use vtkio::Vtk;
    /// use std::path::PathBuf;
    ///
    /// let file_path = PathBuf::from("tet.vtk");
    ///
    /// let mut vtk_file = Vtk::import(&file_path)
    ///     .expect(&format!("Failed to load file: {:?}", file_path));
    /// ```
    pub fn import(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        Vtk::import_impl(file_path.as_ref())
    }

    /// A non-generic helper for the `import` function.
    fn import_impl(path: &Path) -> Result<Vtk, Error> {
        let ext = path
            .extension()
            .and_then(|s| s.to_str())
            .ok_or(Error::UnknownFileExtension(None))?;
        match ext {
            "vtk" => Vtk::import_vtk(path, parser::parse_be),
            #[cfg(feature = "xml")]
            ext => {
                let ft = xml::FileType::try_from_ext(ext)
                    .ok_or(Error::UnknownFileExtension(Some(ext.to_string())))?;
                let vtk_file = xml::import(path)?;
                let exp_ft = xml::FileType::from(vtk_file.data_set_type);
                if ft != exp_ft {
                    Err(Error::XML(xml::Error::TypeExtensionMismatch))
                } else {
                    let mut vtk: Vtk = vtk_file.try_into()?;
                    vtk.file_path = Some(path.into());
                    Ok(vtk)
                }
            }
            #[cfg(not(feature = "xml"))]
            _ => Err(Error::UnknownFileExtension(None)),
        }
    }

    /// Import a VTK file at the specified path.
    ///
    /// This is the async version of [`import`](Vtk::import).
    ///
    /// This function determines the vtk file type from the extension as prescribed by the [VTK
    /// file formats documentation](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/):
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
    /// use vtkio::Vtk;
    /// use std::path::PathBuf;
    ///
    /// let file_path = PathBuf::from("tet.vtk");
    ///
    /// let mut vtk_file = Vtk::import_async(&file_path).await
    ///     .expect(&format!("Failed to load file: {:?}", file_path));
    /// ```
    #[cfg(feature = "async_blocked")]
    pub async fn import_async(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        let path = file_path.as_ref();
        let ext = path.extension().and_then(|s| s.to_str()).ok_or()?;
        match ext {
            "vtk" => import_vtk_async(path, parser::parse_be).await,
            #[cfg(feature = "xml")]
            ext => {
                let ft = xml::FileType::try_from_ext(ext)
                    .ok_or(Error::UnknownFileExtension(Some(ext.to_string())))?;
                let vtk_file = xml::import_async(path).await?;
                let exp_ft = xml::FileType::from(vtk_file.data_set_type);
                if ft != exp_ft {
                    Err(Error::XML(xml::Error::TypeExtensionMismatch))
                } else {
                    Ok(vtk_file.try_into()?)
                }
            }
            #[cfg(not(feature = "xml"))]
            _ => Err(Error::UnknownFileExtension(None)),
        }
    }

    /// Import an XML VTK file in raw form.
    ///
    /// This importer performs a direct translation from the XML string to a Rust representation
    /// without any decoding or conversion. For a more complete import use [`import`].
    ///
    /// [`VTKFile`] is used internally as an intermediate step for constructing the [`Vtk`] model,
    /// which has built-in facilities for loading pieces referenced in "parallel" XML formats as well
    /// as representing Legacy VTK formats, which are more compact when serialized.
    ///
    /// [`Vtk`]: model/struct.Vtk.html
    /// [`VTKFile`]: xml/struct.VTKFile.html
    /// [`import`]: fn.import.html
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
    /// If the file is in binary format, numeric types will be interpreted in little endian format.
    /// For the default byte order used by most `.vtk` files use [`import`] or [`import_legacy_be`].
    ///
    /// [`import`]: fn.import.html
    /// [`import_legacy_be`]: fn.import_legacy_be.html
    pub fn import_legacy_le(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        Vtk::import_vtk(file_path.as_ref(), parser::parse_le)
    }

    #[deprecated(since = "0.6.2", note = "Please use Vtk::import_legacy_le instead")]
    pub fn import_le(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        Vtk::import_legacy_le(file_path.as_ref())
    }

    /// Import a legacy VTK file at the specified path.
    ///
    /// If the file is in binary format, numeric types will be interpreted in big endian format.
    /// This function behaves the same as [`import`], but expects the given file to be strictly in
    /// legacy `.vtk` format.
    ///
    /// [`import`]: fn.import.html
    pub fn import_legacy_be(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        Vtk::import_vtk(file_path.as_ref(), parser::parse_be)
    }

    #[deprecated(since = "0.6.2", note = "Please use Vtk::import_legacy_be instead")]
    pub fn import_be(file_path: impl AsRef<Path>) -> Result<Vtk, Error> {
        Vtk::import_legacy_be(file_path.as_ref())
    }

    /// Export given [`Vtk`] file to the specified file.
    ///
    /// The type of file exported is determined by the extension in `file_path`.
    ///
    /// Files ending in `.vtk` are exported in binary format. For exporting in ASCII, use
    /// [`export_ascii`].
    ///
    /// Endianness is determined by the `byte_order` field of the [`Vtk`] type.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use vtkio::model::*;
    /// use std::path::PathBuf;
    /// let vtk = Vtk {
    ///     version: Version::new((4,1)),
    ///     byte_order: ByteOrder::BigEndian,
    ///     title: String::from("Tetrahedron"),
    ///     file_path: Some(PathBuf::from("./test.vtk")),
    ///     data: DataSet::inline(UnstructuredGridPiece {
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
    /// vtk.export("test.vtk");
    /// ```
    ///
    /// [`Vtk`]: struct.Vtk.html
    /// [`export_ascii`]: fn.export_ascii.html
    pub fn export(self, file_path: impl AsRef<Path>) -> Result<(), Error> {
        self.export_impl(file_path.as_ref())
    }

    /// A non-generic helper for the export function.
    fn export_impl(self, path: &Path) -> Result<(), Error> {
        let ext = path
            .extension()
            .and_then(|s| s.to_str())
            .ok_or(Error::UnknownFileExtension(None))?;
        match ext {
            "vtk" => {
                let file = File::create(path)?;
                BinaryWriter(BufWriter::new(file)).write_vtk(self)?;
                Ok(())
            }
            #[cfg(feature = "xml")]
            ext => {
                let ft = xml::FileType::try_from_ext(ext)
                    .ok_or(Error::UnknownFileExtension(Some(ext.to_string())))?;
                let vtk_file = xml::VTKFile::try_from(self)?;
                let exp_ft = xml::FileType::from(vtk_file.data_set_type);
                if ft != exp_ft {
                    Err(Error::XML(xml::Error::TypeExtensionMismatch))
                } else {
                    xml::export(&vtk_file, path)?;
                    Ok(())
                }
            }
            #[cfg(not(feature = "xml"))]
            _ => Err(Error::UnknownFileExtension(None)),
        }
    }

    /// Write the given VTK file in binary legacy format to the specified [`Write`](std::io::Write)r.
    ///
    /// # Examples
    ///
    /// Writing a binary file in big endian format representing a polygon mesh consisting of a single
    /// triangle:
    ///
    /// ```
    /// use vtkio::model::*; // import model definition of a VTK file
    ///
    /// let mut vtk_bytes = Vec::<u8>::new();
    ///
    /// Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::BigEndian,
    ///     title: String::from("Triangle example"),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::Legacy {
    ///             num_cells: 1,
    ///             vertices: vec![3, 0, 1, 2]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// }.write_legacy(&mut vtk_bytes);
    ///
    /// println!("{}", String::from_utf8_lossy(&vtk_bytes));
    /// ```
    pub fn write_legacy(self, writer: impl std::io::Write) -> Result<(), Error> {
        BinaryWriter(writer).write_vtk(self)?;
        Ok(())
    }

    /// Write the given VTK file in binary legacy format to the specified [`Write`](std::fmt::Write)r.
    ///
    /// # Examples
    ///
    /// Writing an ASCII file representing a polygon mesh consisting of a single triangle:
    ///
    /// ```
    /// use vtkio::model::*; // import model definition of a VTK file
    ///
    /// let mut vtk_string = String::new();
    ///
    /// Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::BigEndian, // Ignored
    ///     title: String::from("Triangle example"),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::Legacy {
    ///             num_cells: 1,
    ///             vertices: vec![3, 0, 1, 2]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// }.write_legacy_ascii(&mut vtk_string);
    ///
    /// assert_eq!(vtk_string.as_str(), "\
    /// ## vtk DataFile Version 2.0
    /// Triangle example
    /// ASCII
    ///
    /// DATASET POLYDATA
    /// POINTS 3 float
    /// 0 0 0 1 0 0 0 0 -1
    ///
    /// POLYGONS 1 4
    /// 3 0 1 2
    ///
    /// POINT_DATA 3
    ///
    /// CELL_DATA 1
    ///
    /// ");
    /// ```
    pub fn write_legacy_ascii(self, writer: impl std::fmt::Write) -> Result<(), Error> {
        AsciiWriter(writer).write_vtk(self)?;
        Ok(())
    }

    /// Write the given VTK file in modern XML format to the specified [`Write`](std::io::Write)r.
    ///
    /// # Examples
    ///
    /// Writing a binary file in big endian format representing a polygon mesh consisting of a single
    /// triangle:
    ///
    /// ```
    /// use vtkio::model::*; // import model definition of a VTK file
    ///
    /// let mut vtk_bytes = Vec::<u8>::new();
    ///
    /// Vtk {
    ///     version: Version::new((2,0)),
    ///     byte_order: ByteOrder::BigEndian,
    ///     title: String::from("Triangle example"),
    ///     file_path: None,
    ///     data: DataSet::inline(PolyDataPiece {
    ///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
    ///         polys: Some(VertexNumbers::Legacy {
    ///             num_cells: 1,
    ///             vertices: vec![3, 0, 1, 2]
    ///         }),
    ///         data: Attributes::new(),
    ///         ..Default::default()
    ///     })
    /// }.write_xml(&mut vtk_bytes);
    ///
    /// assert_eq!(String::from_utf8_lossy(&vtk_bytes), "\
    /// <VTKFile type=\"PolyData\" version=\"2.0\" byte_order=\"BigEndian\" header_type=\"UInt64\">\
    ///   <PolyData>\
    ///     <Piece NumberOfPoints=\"3\" NumberOfLines=\"0\" NumberOfStrips=\"0\" NumberOfPolys=\"1\" NumberOfVerts=\"0\">\
    ///       <PointData/>\
    ///       <CellData/>\
    ///       <Points>\
    ///         <DataArray type=\"Float32\" format=\"binary\" NumberOfComponents=\"3\">\
    ///           AAAAAAAAACQAAAAAAAAAAAAAAAA/gAAAAAAAAAAAAAAAAAAAAAAAAL+AAAA=\
    ///         </DataArray>\
    ///       </Points>\
    ///       <Polys>\
    ///         <DataArray type=\"UInt64\" Name=\"connectivity\" format=\"binary\" NumberOfComponents=\"1\">\
    ///           AAAAAAAAABgAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAI=\
    ///         </DataArray>\
    ///         <DataArray type=\"UInt64\" Name=\"offsets\" format=\"binary\" NumberOfComponents=\"1\">\
    ///           AAAAAAAAAAgAAAAAAAAAAw==\
    ///         </DataArray>\
    ///       </Polys>\
    ///     </Piece>\
    ///   </PolyData>\
    /// </VTKFile>");
    /// ```
    #[cfg(feature = "xml")]
    pub fn write_xml(self, writer: impl Write) -> Result<(), Error> {
        let vtk_file = xml::VTKFile::try_from(self)?;
        xml::write(&vtk_file, writer)?;
        Ok(())
    }

    /// Export the VTK data to the specified path in little endian binary format.
    ///
    /// This function is used as [`export`] but overrides endiannes.
    ///
    /// [`export`]: fn.export.html
    pub fn export_le(self, file_path: impl AsRef<Path>) -> Result<(), Error> {
        let file = File::create(file_path.as_ref())?;
        BinaryWriter(BufWriter::new(file)).write_vtk_le(self)?;
        Ok(())
    }

    /// Export the VTK data to the specified path in big endian binary format.
    ///
    /// This function is used as [`export`] but overrides endiannes.
    ///
    /// [`export`]: fn.export.html
    pub fn export_be(self, file_path: impl AsRef<Path>) -> Result<(), Error> {
        let file = File::create(file_path.as_ref())?;
        BinaryWriter(BufWriter::new(file)).write_vtk_be(self)?;
        Ok(())
    }

    /// Export VTK data to the specified file in ASCII format.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use vtkio::model::*;
    /// use std::path::PathBuf;
    /// let vtk = Vtk {
    ///     version: Version::new((4,1)),
    ///     title: String::from("Tetrahedron"),
    ///     byte_order: ByteOrder::BigEndian,
    ///     file_path: Some(PathBuf::from("./test.vtk")),
    ///     data: DataSet::inline(UnstructuredGridPiece {
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
    /// vtk.export_ascii("test.vtk");
    /// ```
    pub fn export_ascii(self, file_path: impl AsRef<Path>) -> Result<(), Error> {
        // Ascii formats are typically used for small files, so it makes sense to make the write
        // in-memory first.
        let mut out_str = AsciiWriter(String::new());
        out_str.write_vtk(self)?;
        let mut file = File::create(file_path.as_ref())?;
        file.write_all(out_str.0.as_bytes())?;
        Ok(())
    }
}
