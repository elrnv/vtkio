use buffer::IOBuffer;
use std::any::TypeId;
use std::fmt;

/**
 * Vtk Data Model
 */

/// Model of the Vtk data structure.
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
    pub minor: u8
}

impl Version {
    pub fn new(pair: (u8, u8)) -> Self {
        Version { major: pair.0, minor: pair.1 }
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

/// Data structure that stores a Vtk attribute.
#[derive(Clone, PartialEq, Debug)]
pub enum Attribute {
    /// Scalar field. `num_comp` describes how many components (1, 2, 3 or 4) there are
    /// in the field.
    Scalars { num_comp: u8, lookup_table: Option<String>, data: IOBuffer },
    /// `num_comp` is called `nValues` in the Vtk documentation.
    ColorScalars { num_comp: u8, data: IOBuffer },
    LookupTable { data: IOBuffer },
    Vectors { data: IOBuffer },
    /// Normals are assumed to be normalized.
    Normals { data: IOBuffer },
    /// 1D, 2D or 3D texture coordinates are supported by Vtk.
    TextureCoordinates { dim: u8, data: IOBuffer },
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
    Tensors { data: IOBuffer },
    /// Field attribute. Essentially an array of data arrays of any size.
    Field { data_array: Vec<FieldArray> },
}

/// Point and cell attributes.
#[derive(Clone, PartialEq, Debug)]
pub struct Attributes {
    pub point: Vec<(String, Attribute)>,
    pub cell: Vec<(String, Attribute)>,
}

impl Attributes {
    pub fn new() -> Self {
        Attributes {
            point: Vec::new(),
            cell: Vec::new(),
        }
    }
}

/// Cell data. Used in `PolyData` and `UnstructuredGrid` datasets. A cell is just geometric object
/// referencing some points like a polygon or tetrahedron.
/// In general it could be one of VERTICES, LINES, POLYGONS, TRIANGLE_STRIPS or CELLS as defined by
/// the VTK standard.
#[derive(Clone, PartialEq, Debug)]
pub struct Cells {
    /// Total number of cells contained in the `vertices` vector.
    pub num_cells: u32,
    /// Each cell in `vertices` is of the form: `n i_1 ... i_n`.
    pub vertices: Vec<u32>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum PolyDataTopology {
    /// Polygon data toplogy structure for vertices.
    Vertices(Cells),
    /// Polygon data toplogy structure for lines.
    Lines(Cells),
    /// Polygon data toplogy structure for polygons.
    Polygons(Cells),
    /// Polygon data toplogy structure for triangle strips.
    TriangleStrips(Cells),
}

/// This enum describes the types of Cells representable by vtk files. These are explicitly written
/// in `UnstructuredGrid`s and some are referred to in `PolyData` datasets.
/// For more details on each of these types see, the
/// [VTK file formats](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf)
/// documentation or `vtkCell.h` in the vtk SDK.
#[derive(Copy, Clone, PartialEq, Debug, Primitive)]
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
}

/// Dataset described in the file.
/// For 2D objects, `dims[2]` will be set to `0`. For 1D objects, `dims[1]` will also be `0`.
/// This enum is designed to closely represent the data as it is stored in the vtk file.
#[derive(Clone, PartialEq, Debug)]
pub enum DataSet {
    StructuredPoints {
        dims: [u32; 3],
        origin: [f32; 3],
        spacing: [f32; 3],
        data: Attributes
    },
    StructuredGrid {
        dims: [u32; 3],
        points: IOBuffer,
        data: Attributes
    },
    /// 3D Unstructured grid. Note that `cells.num_cells` must equal `cell_types.len()`.
    UnstructuredGrid {
        /// A contiguous array of coordinates (x,y,z) representing the points in the mesh.
        points: IOBuffer,
        cells: Cells,
        cell_types: Vec<CellType>,
        data: Attributes
    },
    /// 3D Polygon data.
    PolyData {
        points: IOBuffer,
        topo: Vec<PolyDataTopology>,
        data: Attributes,
    },
    RectilinearGrid {
        dims: [u32; 3],
        x_coords: IOBuffer,
        y_coords: IOBuffer,
        z_coords: IOBuffer,
        data: Attributes
    },
    /// Same as one field attribute.
    Field {
        name: String,
        data_array: Vec<FieldArray>
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
