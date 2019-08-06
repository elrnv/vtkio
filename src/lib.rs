#[macro_use]
extern crate nom;
extern crate byteorder;
extern crate data_buffer as buffer;
extern crate num_traits;

extern crate num_derive;

#[macro_use]
pub mod basic;

pub mod model;
pub mod parser;
pub mod writer;

use std::io;
use std::path::Path;

pub use buffer::call_numeric_buffer_fn;

/// Primary buffer type used to store data read from binary or ASCII files.
pub type IOBuffer = buffer::DataBuffer;

/// Error type for Import/Export operations.
#[derive(Debug)]
pub enum Error {
    IO(io::Error),
    Write(writer::Error),
    Parse(nom::ErrorKind<u32>),
    Unknown,
}

/// Convert `std::io` error into `vtkio` error.
impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::IO(e)
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

/// Helper function that implements the actual importing routine.
fn import_impl<F>(file_path: &Path, parse: F) -> Result<model::Vtk, Error>
where
    F: Fn(&[u8]) -> nom::IResult<&[u8], model::Vtk>,
{
    use io::Read;
    use nom::IResult;
    use std::fs::File;

    let mut file = File::open(file_path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    match parse(&buf) {
        IResult::Done(_, vtk) => Ok(vtk),
        IResult::Error(e) => Err(Error::Parse(e.into_error_kind())),
        IResult::Incomplete(_) => Err(Error::Unknown),
    }
}

/// Import a vtk file at the specified path.
/// # Examples
/// ```rust,no_run
/// # extern crate vtkio;
/// # use vtkio::model::*;
/// # use vtkio::{import, export_ascii};
/// # fn main() {
/// #    use std::path::PathBuf;
///     let file_path = PathBuf::from("tet.vtk");
///     
///     let mut vtk_file = import(&file_path)
///         .expect(&format!("Failed to load file: {:?}", file_path));
/// # }
/// ```
pub fn import(file_path: &Path) -> Result<model::Vtk, Error> {
    import_impl(file_path, parser::parse)
}

/// Import a vtk file at the specified path. If the file is in binary, numeric types will be
/// interpreted in little-endian format.
pub fn import_le(file_path: &Path) -> Result<model::Vtk, Error> {
    import_impl(file_path, parser::parse_le)
}

/// Import a vtk file at the specified path. If the file is in binary, numeric types will be
/// interpreted in big-endian format.
pub fn import_be(file_path: &Path) -> Result<model::Vtk, Error> {
    import_impl(file_path, parser::parse_be)
}

/// Export given vtk data to the specified file in BINARY format.
/// # Examples
/// ```rust,no_run
/// # extern crate vtkio;
/// # use vtkio::model::*;
/// # use vtkio::export;
/// # fn main() {
/// use std::path::PathBuf;
/// let vtk = Vtk {
///     version: Version::new((4,1)),
///     title: String::from("Tetrahedron"),
///     data: DataSet::UnstructuredGrid {
///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0].into(),
///         cells: Cells { num_cells: 1, vertices: vec![4, 0, 1, 2, 3] },
///         cell_types: vec![CellType::Tetra],
///         data: Attributes::new(),
///     }
/// };
/// export(vtk, &PathBuf::from("test.vtk"));
/// # }
/// ```
pub fn export(data: model::Vtk, file_path: &Path) -> Result<(), Error> {
    use io::Write;
    use writer::WriteVtk;

    let mut file = ::std::fs::File::create(file_path)?;
    file.write_all(Vec::<u8>::new().write_vtk(data)?.as_slice())?;
    Ok(())
}

/// Same as `export` but produces output in little-endian byte order.
pub fn export_le(data: model::Vtk, file_path: &Path) -> Result<(), Error> {
    use io::Write;
    use writer::WriteVtk;

    let mut file = ::std::fs::File::create(file_path)?;
    file.write_all(Vec::<u8>::new().write_vtk_le(data)?.as_slice())?;
    Ok(())
}

/// Same as `export` but produces output in big-endian byte order.
pub fn export_be(data: model::Vtk, file_path: &Path) -> Result<(), Error> {
    use io::Write;
    use writer::WriteVtk;

    let mut file = ::std::fs::File::create(file_path)?;
    file.write_all(Vec::<u8>::new().write_vtk_be(data)?.as_slice())?;
    Ok(())
}

/// Export given vtk data to the specified file in ASCII format.
/// # Examples
/// ```rust,no_run
/// # extern crate vtkio;
/// # use vtkio::model::*;
/// # use vtkio::export_ascii;
/// # fn main() {
/// use std::path::PathBuf;
/// let vtk = Vtk {
///     version: Version::new((4,1)),
///     title: String::from("Tetrahedron"),
///     data: DataSet::UnstructuredGrid {
///         points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0].into(),
///         cells: Cells { num_cells: 1, vertices: vec![4, 0, 1, 2, 3] },
///         cell_types: vec![CellType::Tetra],
///         data: Attributes::new(),
///     }
/// };
/// export_ascii(vtk, &PathBuf::from("test.vtk"));
/// # }
/// ```
pub fn export_ascii(data: model::Vtk, file_path: &Path) -> Result<(), Error> {
    use io::Write;
    use std::fs::File;
    use writer::WriteVtk;

    let mut out_str = String::new();
    out_str.write_vtk(data)?;
    {
        let mut file = File::create(file_path)?;
        file.write_all(out_str.as_bytes())?;
    }
    Ok(())
}
