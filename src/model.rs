use crate::IOBuffer;
use num_derive::FromPrimitive;
use std::any::TypeId;
use std::fmt;
use std::ops::RangeInclusive;

/**
 * VTK Data Model
 */

/// Error type describing failure modes of various model processing tasks and validation.
#[derive(Debug)]
pub enum Error {
    FailedToLoadPieceData,
    MissingPieceData,
    IO(std::io::Error),
    VTKIO(Box<crate::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::MissingPieceData => write!(f, "Missing piece data"),
            Error::IO(source) => write!(f, "IO error: {:?}", source),
            Error::VTKIO(source) => write!(f, "VTK IO error: {:?}", source),
            Error::FailedToLoadPieceData => write!(f, "Failed to load piece data"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IO(source) => Some(source),
            Error::VTKIO(source) => Some(source),
            Error::FailedToLoadPieceData => None,
            Error::MissingPieceData => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Error {
        Error::IO(e)
    }
}

impl From<crate::Error> for Error {
    fn from(e: crate::Error) -> Error {
        Error::VTKIO(Box::new(e))
    }
}

/// Model of the VTK data structure.
#[derive(Clone, PartialEq, Debug)]
pub struct Vtk {
    pub version: Version,
    pub title: String,
    pub data: DataSet,
}

/// Version number (e.g. `4.1 => (4,1)`)
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Version {
    pub major: u8,
    pub minor: u8,
}

impl Version {
    pub fn new(pair: (u8, u8)) -> Self {
        Version {
            major: pair.0,
            minor: pair.1,
        }
    }
}

impl From<(u8, u8)> for Version {
    fn from(pair: (u8, u8)) -> Self {
        Version::new(pair)
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

/// Special struct to store Field attribute data.
#[derive(Clone, PartialEq, Debug)]
pub struct FieldArray {
    pub name: String,
    pub num_comp: u32,
    pub data: IOBuffer,
}

/// Data structure that stores a VTK attribute.
#[derive(Clone, PartialEq, Debug)]
pub enum Attribute {
    /// Scalar field. `num_comp` describes how many components (1, 2, 3 or 4) there are
    /// in the field.
    Scalars {
        num_comp: u8,
        lookup_table: Option<String>,
        data: IOBuffer,
    },
    /// `num_comp` is called `nValues` in the VTK documentation.
    ColorScalars {
        num_comp: u8,
        data: IOBuffer,
    },
    LookupTable {
        data: IOBuffer,
    },
    Vectors {
        data: IOBuffer,
    },
    /// Normals are assumed to be normalized.
    Normals {
        data: IOBuffer,
    },
    /// 1D, 2D or 3D texture coordinates are supported by VTK.
    TextureCoordinates {
        dim: u8,
        data: IOBuffer,
    },
    /// 3x3 symmetric tensors are supported. These are given in full row major form:
    /// ```verbatim
    ///     [t^1_00, t^1_01, t^1_02,
    ///      t^1_10, t^1_11, t^1_12,
    ///      t^1_20, t^1_21, t^1_22,
    ///      ...
    ///      t^{n}_00, t^{n}_01, t^{n}_02,
    ///      t^{n}_10, t^{n}_11, t^{n}_12,
    ///      t^{n}_20, t^{n}_21, t^{n}_22,
    ///     ]
    /// ```
    /// Note that symmetry is assumed (`t^k_ij == t^k_ji`).
    Tensors {
        data: IOBuffer,
    },
    /// Field attribute. Essentially an array of data arrays of any size.
    Field {
        data_array: Vec<FieldArray>,
    },
}

/// A named attribute, represented as an array of typed data.
#[derive(Clone, PartialEq, Debug)]
pub struct DataArray {
    pub name: String, 
    pub data: Attribute,
}

impl From<(String, Attribute)> for DataArray {
    fn from((s, a): (String, Attribute)) -> DataArray {
        DataArray {
            name: s,
            data: a,
        }
    }
}

/// Point and cell attributes.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct Attributes {
    pub point: Vec<DataArray>,
    pub cell: Vec<DataArray>,
}

impl Attributes {
    pub fn new() -> Self {
        Default::default()
    }
}

/// Vertex numbers for general cells, polygons, lines, strips or stand-alone vertices.
///
/// Used in `PolyData` and `UnstructuredGrid` datasets. Below we refer to a cell as just a
/// geometric object referencing points like a polygon or tetrahedron.
///
/// This struct compiles a list of point indices that make up each cell.
///
/// # Legacy
///
/// In legacy format, cell vertex numbers are listed with a preceeding number of points per cell.
/// In other words, each cell's point list is given by a number of points in the cell followed by
/// the individual point numbers.
/// This struct could represent one of VERTICES, LINES, POLYGONS, TRIANGLE_STRIPS or CELLS.
///
/// # XML
///
/// In XML format, the cell vertex numbers listed as a contiguous array, so to distinguish between
/// different cells, a secondary array of offsets is given to indicate the ends of each cell as an
/// index into the vertex array. This struct represents a portion of the `Cells` element or one of
/// `Verts`, `Lines`, `Strips` or `Polys`.
#[derive(Clone, PartialEq, Debug)]
pub enum VertexNumbers {
    Legacy {
        /// Total number of cells contained in the `vertices` vector.
        num_cells: u32,
        /// Each cell in `vertices` is of the form: `n i_1 ... i_n`.
        vertices: Vec<u32>,
    },
    XML {
        /// A contiguous array of all of the cells' point lists concatenated together.
        connectivity: Vec<u32>,
        /// The offsets into the connectivity array indicating the end of each cell.
        offsets: Vec<u32>,
    }
}

impl VertexNumbers {
    /// Returns the total number of vertices among all the cells.
    #[inline]
    pub fn num_verts(&self) -> usize {
        match self {
            VertexNumbers::Legacy { vertices, num_cells } => vertices.len() - *num_cells as usize,
            VertexNumbers::XML { connectivity, .. } => connectivity.len(),
        }
    }

    /// Returns the total number of cells represented by these vertex numbers.
    #[inline]
    pub fn num_cells(&self) -> usize {
        match self {
            VertexNumbers::Legacy { num_cells, .. } => *num_cells as usize,
            VertexNumbers::XML { offsets, .. } => offsets.len(),
        }
    }

    /// Converts `self` into `Legacy` format.
    ///
    /// Returns a number of cells and vertices array pair as in the `Legacy` variant.
    pub fn into_legacy(self) -> (u32, Vec<u32>) {
        match self {
            VertexNumbers::Legacy { num_cells, vertices } => {
                (num_cells, vertices)
            }
            VertexNumbers::XML { connectivity, offsets } => {
                let num_cells = offsets.len();
                let num_verts = connectivity.len();
                let mut vertices = Vec::with_capacity(num_verts + num_cells);
                let mut i = 0u32;
                for off in offsets.into_iter() {
                    vertices.push(off - i);
                    while i < off {
                        vertices.push(connectivity[i as usize]);
                        i += 1;
                    }
                }
                (num_cells as u32, vertices)
            }
        }
    }
}

/// Cells with variable types.
///
/// This struct corresponds to the `Cells` XML element or the CELLS and CELL_TYPES entries in the
/// legacy VTK format.
#[derive(Clone, PartialEq, Debug)]
pub struct Cells {
    /// Cell vertices specified through offsets or simply as a contiguous array.
    ///
    /// See [`VertexNumbers`] for details.
    /// 
    /// [`VertexNumbers`]: struct.VertexNumbers.html
    pub cell_verts: VertexNumbers,
    /// The type of each cell represented in `cell_verts`.
    pub types: Vec<CellType>,
}

impl Cells {
    /// Returns the total number of vertices among all the cells.
    #[inline]
    pub fn num_verts(&self) -> usize {
        self.cell_verts.num_verts()
    }
    /// Returns the total number of cells represented.
    #[inline]
    pub fn num_cells(&self) -> usize {
        self.types.len()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum PolyDataTopology {
    /// Polygon data toplogy structure for vertices.
    Vertices(VertexNumbers),
    /// Polygon data toplogy structure for lines.
    Lines(VertexNumbers),
    /// Polygon data toplogy structure for polygons.
    Polygons(VertexNumbers),
    /// Polygon data toplogy structure for triangle strips.
    TriangleStrips(VertexNumbers),
}

impl PolyDataTopology {
    pub fn num_cells(&self) -> usize {
        match self {
            PolyDataTopology::Vertices(vert_nums) |
            PolyDataTopology::Lines(vert_nums) |
            PolyDataTopology::Polygons(vert_nums) |
            PolyDataTopology::TriangleStrips(vert_nums) => {
                vert_nums.num_cells()
            }
        }
    }
}

/// This enum describes the types of Cells representable by vtk files.
///
/// These are explicitly written in `UnstructuredGrid`s and some are referred to in `PolyData`
/// datasets.  For more details on each of these types see, the [VTK file
/// formats](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf) documentation or
/// `vtkCell.h` in the vtk SDK.
#[derive(Copy, Clone, PartialEq, Debug, FromPrimitive)]
pub enum CellType {
    Vertex = 1,
    PolyVertex = 2,
    Line = 3,
    PolyLine = 4,
    Triangle = 5,
    TriangleStrip = 6,
    Polygon = 7,
    Pixel = 8,
    Quad = 9,
    Tetra = 10,
    Voxel = 11,
    Hexahedron = 12,
    Wedge = 13,
    Pyramid = 14,
    QuadraticEdge = 21,
    QuadraticTriangle = 22,
    QuadraticQuad = 23,
    QuadraticTetra = 24,
    QuadraticHexahedron = 25,
}

/// Point coordinates on a `RectilinearGrid` corresponding to `x`, `y` and `z` axes.
///
/// Coordinates for an extent are specified by the ordinate along each axis for each integer value
/// in the extent’s range. This contains three `IOBuffer`s describing the ordinates along
/// the x-y-z axes, respectively.
///
/// This struct corresponds to the `Coordinates` element in XML formats.
#[derive(Clone, Debug, PartialEq)]
pub struct Coordinates {
    /// Point coordinates along the `x` axis.
    pub x: IOBuffer,
    /// Point coordinates along the `y` axis.
    pub y: IOBuffer,
    /// Point coordinates along the `z` axis.
    pub z: IOBuffer,
}

/// The extent of the structured object being represented in 3D space.
#[derive(Clone, PartialEq, Debug)]
pub enum Extent {
    /// Legacy formats use dimensions to indicate the extent of a grid.
    Dims([u32; 3]),
    /// In XML format, inclusive ranges are given as a 6-tuple:
    ///
    /// `[ x0 x1 y0 y1 z0 z1 ]`
    ///
    /// where the extent of the grid in say `x` is given by the inclusive range `x0..=x1`.
    ///
    /// These are translated into Rust's `RangeInclusive` for explicitness and convenience as
    ///
    /// `[ x0..=x1, y0..=y1, z0..=z1 ]`
    ///
    /// The equivalent extent in legacy format would be `Dims([x1-x0+1, y1-y0+1, z1-z0+1])`.
    Ranges(RangeExtent),
}

/// An extent for structured data specified as a triplet of inclusive ranges.
///
/// For example `[ x0..=x1, y0..=y1, z0..=z1 ]` gives the extent of a data set between `x0` and
/// `x1` in the `x` dimension and similar for `y` and `z`.
pub type RangeExtent = [RangeInclusive<u32>; 3];

impl Extent {
    /// Convert `Extent` to a triple of dimensions.
    ///
    /// If the extent is stored as `Extent::Ranges` such as
    ///
    /// `[ x0..=x1, y0..=y1, z0..=z1 ]`
    ///
    /// then the equivalent extent in legacy format is returned:
    ///
    /// `[x1-x0+1, y1-y0+1, z1-z0+1]`
    pub fn into_dims(self) -> [u32; 3] {
        match self {
            Extent::Dims(dims) => dims,
            Extent::Ranges([x, y, z]) => {
                [x.end()-x.start()+1, y.end()-y.start()+1, z.end()-z.start()+1]
            }
        }
    }
}

/// A piece of a data set.
///
/// This can be stored as a reference to another VTK file, as pointer to memory with the
/// corresponding piece data set, or as inline piece data as described in serial XML formats or
/// legacy formats.
#[derive(Clone, Debug, PartialEq)]
pub enum Piece {
    /// A reference to a piece as a file path.
    ///
    /// This variant is used with "Parallel" XML formats, which distribute their data among a
    /// collection of other files storing pieces of the data.
    Source(String),
    /// Data set corresponding to piece data loaded from a file.
    ///
    /// This variant is when data referenced in "Parallel" XML formats, gets loaded.
    Loaded(Box<DataSet>),
    /// Piece data stored inline with the rest of the host file.
    ///
    /// This corresponds to `Piece` elements stored in serial XML formats.
    Inline(Box<PieceData>),
}

impl Piece {
    /// Converts `self` to loaded piece data.
    /// 
    /// If the piece is not yet loaded, this funciton will load it and return the reference to the
    /// resulting data.
    pub fn load_into_piece_data(mut self) -> Result<PieceData, Error> {
        match self {
            Piece::Source(path) => {
                let piece_vtk = crate::import(&path)?;
                let piece = Box::new(piece_vtk.data);
                self = Piece::Loaded(piece);
                self.load_into_piece_data()
            },
            Piece::Loaded(data_set) => {
                match *data_set {
                    DataSet::ImageData { pieces, .. } |
                    DataSet::StructuredGrid { pieces, .. } | 
                    DataSet::RectilinearGrid { pieces, .. } | 
                    DataSet::UnstructuredGrid { pieces, .. } | 
                    DataSet::PolyData { pieces, .. } => {
                        pieces.into_iter().next().ok_or(Error::MissingPieceData)?.load_into_piece_data()
                    }
                    _ => return Err(Error::MissingPieceData),
                }
            },
            Piece::Inline(piece_data) => {
                Ok(*piece_data)
            },
        }
    }
}

/// Storage for piece data.
#[derive(Clone, Debug, PartialEq)]
pub enum PieceData {
    ImageData {
        extent: Extent,
        data: Attributes,
    },
    RectilinearGrid {
        extent: Extent,
        coords: Coordinates,
        data: Attributes,
    },
    StructuredGrid {
        extent: Extent,
        points: IOBuffer, 
        data: Attributes,
    },
    /// PolyData piece data.
    ///
    /// For XML formats, to get the corresponding `NumberOfVerts`, `NumberOfLines` etc. use the
    /// `num_cells` function of `PolyDataTopology`, which will give the appropriate number
    /// depending on the type of geometry. To get `NumberOfPoints`, simply take the length of
    /// `points`.
    PolyData {
        points: IOBuffer,
        topo: Vec<PolyDataTopology>,
        data: Attributes,
    },
    /// UnstructuredGrid piece data.
    UnstructuredGrid {
        points: IOBuffer,
        cells: Cells,
        data: Attributes,
    }
}

/// Dataset described in the file.
///
/// For 2D objects, `dims[2]` will be set to `1`. For 1D objects, `dims[1]` will also be `1`.
/// This enum is designed to closely represent the data as it is stored in the vtk file.
///
/// The `extent` specified in the enum variants corresponds to the `WholeExtent` attribute.
///
/// Each `DataSet` is split into pieces for compatibility with XML formats. Legacy formats
/// correspond to a data set with a single inline piece.
#[derive(Clone, PartialEq, Debug)]
pub enum DataSet {
    /// Also referred to as `StructuredPoints` in Legacy format.
    ImageData {
        extent: Extent,
        origin: [f32; 3],
        spacing: [f32; 3],
        pieces: Vec<Piece>,
    },
    StructuredGrid {
        extent: Extent,
        pieces: Vec<Piece>,
    },
    RectilinearGrid {
        extent: Extent,
        pieces: Vec<Piece>,
    },
    /// 3D Unstructured grid. Note that `cells.num_cells` must equal `cell_types.len()`.
    UnstructuredGrid {
        /// A contiguous array of coordinates (x,y,z) representing the points in the mesh.
        pieces: Vec<Piece>,
    },
    /// 3D Polygon data.
    PolyData {
        pieces: Vec<Piece>,
    },
    /// Same as one field attribute.
    Field {
        name: String,
        data_array: Vec<FieldArray>,
    },
}

impl DataSet {
    /// Construct a one piece data set.
    ///
    /// When creating an `ImageData` set, the default origin is `[0.0; 3]` and spacing `[1.0; 3]` is
    /// used.
    pub fn inline(p: PieceData) -> DataSet {
        match &p {
            PieceData::ImageData { extent, .. } => {
                DataSet::ImageData { 
                    extent: extent.clone(),
                    origin: [0.0; 3],
                    spacing: [1.0; 3],
                    pieces: vec![Piece::Inline(Box::new(p))],
                }
            }
            PieceData::StructuredGrid { extent, .. } => {
                DataSet::StructuredGrid {
                    extent: extent.clone(),
                    pieces: vec![Piece::Inline(Box::new(p))],
                }
            }
            PieceData::RectilinearGrid { extent, .. } => {
                DataSet::RectilinearGrid {
                    extent: extent.clone(),
                    pieces: vec![Piece::Inline(Box::new(p))],
                }
            }
            PieceData::UnstructuredGrid { .. } => {
                DataSet::UnstructuredGrid {
                    pieces: vec![Piece::Inline(Box::new(p))],
                }
            }
            PieceData::PolyData { .. } => {
                DataSet::PolyData {
                    pieces: vec![Piece::Inline(Box::new(p))],
                }
            }
        }
    }
}

/// Types of data that can be recognized by the parser. Not all data types are supported for all
/// classes.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum DataType {
    Bit,
    UnsignedChar,
    Char,
    UnsignedShort,
    Short,
    UnsignedInt,
    Int,
    UnsignedLong,
    Long,
    Float,
    Double,
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DataType::Bit => write!(f, "bit"),
            DataType::UnsignedChar => write!(f, "unsigned_char"),
            DataType::Char => write!(f, "char"),
            DataType::UnsignedShort => write!(f, "unsigned_short"),
            DataType::Short => write!(f, "short"),
            DataType::UnsignedInt => write!(f, "unsigned_int"),
            DataType::Int => write!(f, "int"),
            DataType::UnsignedLong => write!(f, "unsigned_long"),
            DataType::Long => write!(f, "long"),
            DataType::Float => write!(f, "float"),
            DataType::Double => write!(f, "double"),
        }
    }
}

impl From<TypeId> for DataType {
    fn from(dt: TypeId) -> Self {
        match dt {
            x if x == TypeId::of::<u8>() => DataType::UnsignedChar,
            x if x == TypeId::of::<i8>() => DataType::Char,
            x if x == TypeId::of::<u16>() => DataType::UnsignedShort,
            x if x == TypeId::of::<i16>() => DataType::Short,
            x if x == TypeId::of::<u32>() => DataType::UnsignedInt,
            x if x == TypeId::of::<i32>() => DataType::Int,
            x if x == TypeId::of::<u64>() => DataType::UnsignedLong,
            x if x == TypeId::of::<i64>() => DataType::Long,
            x if x == TypeId::of::<f32>() => DataType::Float,
            x if x == TypeId::of::<f64>() => DataType::Double,
            _ => panic!("Specified type is unsupported by VTK."),
        }
    }
}
