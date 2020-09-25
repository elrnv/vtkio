use std::any::TypeId;
use std::convert::TryFrom;
use std::fmt;
use std::ops::RangeInclusive;

use bytemuck::cast_vec;
use num_derive::FromPrimitive;

/**
 * VTK Data Model
 */

/// Error type describing failure modes of various model processing tasks and validation.
#[derive(Debug)]
pub enum Error {
    InvalidCast(std::io::Error),
    FailedToLoadPieceData,
    MissingPieceData,
    IO(std::io::Error),
    VTKIO(Box<crate::Error>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::InvalidCast(source) => write!(f, "Invalid cast error: {:?}", source),
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
            Error::InvalidCast(source) => Some(source),
            Error::IO(source) => Some(source),
            Error::VTKIO(source) => Some(source),
            _ => None,
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
    pub byte_order: ByteOrder,
    pub data: DataSet,
}

/// Version number (e.g. `4.1 => Version { major: 4, minor: 1 }`)
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

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ByteOrder {
    BigEndian,
    LittleEndian,
}

/// Data loaded from either binary or ascii format.
#[derive(Clone, PartialEq, Debug)]
pub enum IOBuffer {
    /// Bit array is stored in 8 bit chunks.
    Bit(Vec<u8>),
    /// Vector of unsigned bytes.
    U8(Vec<u8>),
    /// Vector of signed bytes.
    I8(Vec<i8>),
    /// Vector of unsigned short integers `u16`.
    U16(Vec<u16>),
    /// Vector of signed short integers `i16`.
    I16(Vec<i16>),
    /// Vector of unsigned integers `u32`.
    U32(Vec<u32>),
    /// Vector of signed integers `i32`.
    I32(Vec<i32>),
    /// Vector of unsigned long integers `u64`.
    U64(Vec<u64>),
    /// Vector of signed long integers `i64`.
    I64(Vec<i64>),
    /// Vector of single precision floats.
    F32(Vec<f32>),
    /// Vector of double precision floats.
    F64(Vec<f64>),
}

impl Default for IOBuffer {
    fn default() -> IOBuffer {
        IOBuffer::F32(Vec::new())
    }
}

macro_rules! impl_io_buffer_convert {
    ($t:ident <=> $v:ident) => {
        impl From<Vec<$t>> for IOBuffer {
            fn from(v: Vec<$t>) -> IOBuffer {
                IOBuffer::$v(v)
            }
        }

        impl Into<Option<Vec<$t>>> for IOBuffer {
            fn into(self) -> Option<Vec<$t>> {
                if let IOBuffer::$v(v) = self {
                    Some(v)
                } else {
                    None
                }
            }
        }

        impl std::iter::FromIterator<$t> for IOBuffer {
            fn from_iter<T>(iter: T) -> Self
            where
                T: IntoIterator<Item = $t>,
            {
                iter.into_iter().collect::<Vec<$t>>().into()
            }
        }
    };
}

impl_io_buffer_convert!(u8 <=> U8);
impl_io_buffer_convert!(i8 <=> I8);
impl_io_buffer_convert!(u16 <=> U16);
impl_io_buffer_convert!(i16 <=> I16);
impl_io_buffer_convert!(u32 <=> U32);
impl_io_buffer_convert!(i32 <=> I32);
impl_io_buffer_convert!(u64 <=> U64);
impl_io_buffer_convert!(i64 <=> I64);
impl_io_buffer_convert!(f32 <=> F32);
impl_io_buffer_convert!(f64 <=> F64);

/// Evaluate the expression `$e` given a `Vec` `$v`.
#[macro_export]
macro_rules! match_buf {
    ($buf:expr; $v:pat => $e:expr) => {
        match $buf {
            IOBuffer::Bit($v) => $e,
            IOBuffer::U8($v) => $e,
            IOBuffer::I8($v) => $e,
            IOBuffer::U16($v) => $e,
            IOBuffer::I16($v) => $e,
            IOBuffer::U32($v) => $e,
            IOBuffer::I32($v) => $e,
            IOBuffer::U64($v) => $e,
            IOBuffer::I64($v) => $e,
            IOBuffer::F32($v) => $e,
            IOBuffer::F64($v) => $e,
        }
    };
}

macro_rules! impl_bytes_constructor {
    ($bytes:ident, $bo:ident, $read:ident, $t:ident, $variant:ident) => {{
        use byteorder::ReadBytesExt;
        let mut out = vec![num_traits::Zero::zero(); $bytes.len() / std::mem::size_of::<$t>()];
        let mut reader = std::io::Cursor::new($bytes);
        match $bo {
            ByteOrder::BigEndian => reader
                .$read::<byteorder::BE>(out.as_mut_slice())
                .map_err(|e| Error::InvalidCast(e))?,
            ByteOrder::LittleEndian => reader
                .$read::<byteorder::LE>(out.as_mut_slice())
                .map_err(|e| Error::InvalidCast(e))?,
        }
        Ok(IOBuffer::$variant(out))
    }};
}

impl IOBuffer {
    pub fn scalar_type(&self) -> ScalarType {
        match self {
            IOBuffer::Bit(_) => ScalarType::Bit,
            IOBuffer::U8(_) => ScalarType::U8,
            IOBuffer::I8(_) => ScalarType::I8,
            IOBuffer::U16(_) => ScalarType::U16,
            IOBuffer::I16(_) => ScalarType::I16,
            IOBuffer::U32(_) => ScalarType::U32,
            IOBuffer::I32(_) => ScalarType::I32,
            IOBuffer::U64(_) => ScalarType::U64,
            IOBuffer::I64(_) => ScalarType::I64,
            IOBuffer::F32(_) => ScalarType::F32,
            IOBuffer::F64(_) => ScalarType::F64,
        }
    }

    pub fn len(&self) -> usize {
        match_buf!(self; v => v.len())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn into_bytes(self, bo: ByteOrder) -> Vec<u8> {
        use byteorder::WriteBytesExt;
        use byteorder::{BE, LE};
        let mut out: Vec<u8> = Vec::new();

        match self {
            IOBuffer::Bit(v) => out = v,
            IOBuffer::U8(v) => out = v,
            IOBuffer::I8(v) => out = cast_vec(v),
            IOBuffer::U16(v) => {
                out.reserve(v.len() * std::mem::size_of::<u16>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_u16::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_u16::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::I16(v) => {
                out.reserve(v.len() * std::mem::size_of::<i16>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_i16::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_i16::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::U32(v) => {
                out.reserve(v.len() * std::mem::size_of::<u32>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_u32::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_u32::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::I32(v) => {
                out.reserve(v.len() * std::mem::size_of::<i32>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_i32::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_i32::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::U64(v) => {
                out.reserve(v.len() * std::mem::size_of::<u64>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_u64::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_u64::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::I64(v) => {
                out.reserve(v.len() * std::mem::size_of::<i64>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_i64::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_i64::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::F32(v) => {
                out.reserve(v.len() * std::mem::size_of::<f32>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_f32::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_f32::<LE>(x).unwrap())
                    }
                }
            }
            IOBuffer::F64(v) => {
                out.reserve(v.len() * std::mem::size_of::<f64>());
                match bo {
                    ByteOrder::BigEndian => {
                        v.into_iter().for_each(|x| out.write_f64::<BE>(x).unwrap())
                    }
                    ByteOrder::LittleEndian => {
                        v.into_iter().for_each(|x| out.write_f64::<LE>(x).unwrap())
                    }
                }
            }
        }
        out
    }

    /// Construct an `IOBuffer` from a `Vec` of bytes and a corresponding scalar type.
    pub fn from_bytes(
        bytes: Vec<u8>,
        scalar_type: ScalarType,
        bo: ByteOrder,
    ) -> Result<Self, Error> {
        match scalar_type {
            ScalarType::Bit => Ok(IOBuffer::u8_from_bytes(bytes)),
            ScalarType::I8 => Ok(IOBuffer::i8_from_bytes(bytes)),
            ScalarType::U8 => Ok(IOBuffer::u8_from_bytes(bytes)),
            ScalarType::I16 => IOBuffer::i16_from_bytes(bytes, bo),
            ScalarType::U16 => IOBuffer::u16_from_bytes(bytes, bo),
            ScalarType::I32 => IOBuffer::i32_from_bytes(bytes, bo),
            ScalarType::U32 => IOBuffer::u32_from_bytes(bytes, bo),
            ScalarType::I64 => IOBuffer::i64_from_bytes(bytes, bo),
            ScalarType::U64 => IOBuffer::u64_from_bytes(bytes, bo),
            ScalarType::F32 => IOBuffer::f32_from_bytes(bytes, bo),
            ScalarType::F64 => IOBuffer::f64_from_bytes(bytes, bo),
        }
    }

    /// Construct an `IOBuffer` with `u8` elements from the given `Vec` of bytes.
    pub fn u8_from_bytes(bytes: Vec<u8>) -> Self {
        // Nothing to do here
        IOBuffer::U8(bytes)
    }
    /// Construct an `IOBuffer` with `i8` elements from the given `Vec` of bytes.
    pub fn i8_from_bytes(bytes: Vec<u8>) -> Self {
        IOBuffer::I8(cast_vec(bytes))
    }

    /// Construct an `IOBuffer` with `u16` elements from the given `Vec` of bytes.
    pub fn u16_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_u16_into, u16, U16)
    }
    /// Construct an `IOBuffer` with `i16` elements from the given `Vec` of bytes.
    pub fn i16_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_i16_into, i16, I16)
    }
    /// Construct an `IOBuffer` with `u32` elements from the given `Vec` of bytes.
    pub fn u32_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_u32_into, u32, U32)
    }
    /// Construct an `IOBuffer` with `i32` elements from the given `Vec` of bytes.
    pub fn i32_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_i32_into, i32, I32)
    }
    /// Construct an `IOBuffer` with `u64` elements from the given `Vec` of bytes.
    pub fn u64_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_u64_into, u64, U64)
    }
    /// Construct an `IOBuffer` with `i64` elements from the given `Vec` of bytes.
    pub fn i64_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_i64_into, i64, I64)
    }
    /// Construct an `IOBuffer` with `f32` elements from the given `Vec` of bytes.
    pub fn f32_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_f32_into, f32, F32)
    }
    /// Construct an `IOBuffer` with `f64` elements from the given `Vec` of bytes.
    pub fn f64_from_bytes(bytes: Vec<u8>, bo: ByteOrder) -> Result<Self, Error> {
        impl_bytes_constructor!(bytes, bo, read_f64_into, f64, F64)
    }

    /// Returns an iterator over elements with type `T`.
    ///
    /// If `T` is not one of `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `f32`, or `f64`,
    /// then `None` is returned.
    pub fn iter<T: Scalar>(&self) -> Option<std::slice::Iter<T>> {
        T::io_buf_vec_ref(self).map(|v| v.iter())
    }

    /// Converts this buffer into the underlying `Vec` representation.
    ///
    /// If `T` is not one of `u8`, `i8`, `u16`, `i16`, `u32`, `i32`, `u64`, `i64`, `f32`, or `f64`,
    /// then `None` is returned.
    pub fn into_vec<T: Scalar>(self) -> Option<Vec<T>> {
        T::io_buf_into_vec(self)
    }
}

pub trait Scalar
where
    Self: Sized,
{
    fn io_buf_vec_ref(io_buf: &IOBuffer) -> Option<&Vec<Self>>;
    fn io_buf_into_vec(io_buf: IOBuffer) -> Option<Vec<Self>>;
}

macro_rules! impl_scalar {
    ($t:ident, $v:ident) => {
        impl Scalar for $t {
            fn io_buf_vec_ref(io_buf: &IOBuffer) -> Option<&Vec<Self>> {
                match io_buf {
                    IOBuffer::$v(v) => Some(v),
                    _ => None,
                }
            }
            fn io_buf_into_vec(io_buf: IOBuffer) -> Option<Vec<Self>> {
                match io_buf {
                    IOBuffer::$v(v) => Some(v),
                    _ => None,
                }
            }
        }
    };
}

impl_scalar!(u8, U8);
impl_scalar!(i8, I8);
impl_scalar!(u16, U16);
impl_scalar!(i16, I16);
impl_scalar!(u32, U32);
impl_scalar!(i32, I32);
impl_scalar!(u64, U64);
impl_scalar!(i64, I64);
impl_scalar!(f32, F32);
impl_scalar!(f64, F64);

impl std::fmt::Display for IOBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match_buf!(self; v => {
            let mut iter = v.iter();
            if let Some(next) = iter.next() {
                write!(f, "{}", next)?;
                for i in iter {
                    write!(f, " {}", i)?;
                }
            }
        });
        Ok(())
    }
}

/// A named array of elements.
///
/// This is stored as contiguous chunks of components represening and element described by
/// `elem`.
#[derive(Clone, PartialEq, Debug)]
pub struct DataArrayBase<E> {
    /// The name of the data array.
    pub name: String,
    /// The type of element stored by the `data` storage buffer.
    pub elem: E,
    /// A contiguous typed storage buffer.
    ///
    /// This stores the actual attribute values in an appropriately typed vector.
    pub data: IOBuffer,
}

/// A data array whose elements have a number of components given by the integer `elem`.
///
/// This is the most "unopinionated" version of a `DataArrayBase` in that it doesn't assume a
/// purpose for the associated buffer.
pub type FieldArray = DataArrayBase<u32>;

/// A data array whose elements are given a type by `elem`.
///
/// This is the most general version of a `DataArrayBase` in that it also labels the buffer with a
/// particular purpose (e.g. colors, texture coordinates).
pub type DataArray = DataArrayBase<ElementType>;

impl Default for DataArray {
    fn default() -> DataArray {
        DataArray {
            name: String::new(),
            elem: ElementType::default(),
            data: IOBuffer::default(),
        }
    }
}

impl Default for FieldArray {
    fn default() -> FieldArray {
        FieldArray {
            name: String::new(),
            elem: 1,
            data: IOBuffer::default(),
        }
    }
}

impl From<IOBuffer> for DataArray {
    fn from(buf: IOBuffer) -> DataArray {
        DataArray {
            name: String::new(),
            elem: ElementType::Generic(1),
            data: buf,
        }
    }
}

impl<E> DataArrayBase<E> {
    /// Get the scalar data type stored by the underlying buffer.
    pub fn scalar_type(&self) -> ScalarType {
        self.data.scalar_type()
    }
    /// Get the number of elements stored by this data array.
    ///
    /// This is equal to `self.len() / self.num_comp()`.
    pub fn num_elem(&self) -> usize {
        self.data.len()
    }
    /// Get the raw length of the underlying buffer.
    ///
    /// This is equal to `self.num_elem() * self.num_comp()`.
    pub fn len(&self) -> usize {
        self.data.len()
    }
    /// Returns `true` if this data array is empty and `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Set the data of this named array to the given buffer.
    ///
    /// If the data was previously already set, it will be overwritten with the one given in this
    /// function.
    pub fn with_data(mut self, data: impl Into<IOBuffer>) -> Self {
        self.data = data.into();
        self
    }
}

impl DataArray {
    /// Construct an empty scalars array with the given lookup table.
    pub fn scalars_with_lookup(
        name: impl Into<String>,
        num_comp: u32,
        lookup_table: impl Into<String>,
    ) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Scalars {
                num_comp,
                lookup_table: Some(lookup_table.into()),
            },
            ..Default::default()
        }
    }
    /// Construct an empty scalars array.
    pub fn scalars(name: impl Into<String>, num_comp: u32) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Scalars {
                num_comp,
                lookup_table: None,
            },
            ..Default::default()
        }
    }
    /// Construct an empty color scalars array.
    pub fn color_scalars(name: impl Into<String>, num_comp: u32) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::ColorScalars(num_comp),
            ..Default::default()
        }
    }
    /// Construct an empty lookup table.
    pub fn lookup_table(name: impl Into<String>) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::LookupTable,
            ..Default::default()
        }
    }
    /// Construct an empty vectors array.
    pub fn vectors(name: impl Into<String>) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Vectors,
            ..Default::default()
        }
    }
    /// Construct an empty normals array.
    pub fn normals(name: impl Into<String>) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Normals,
            ..Default::default()
        }
    }
    /// Construct an empty tensors array.
    pub fn tensors(name: impl Into<String>) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Tensors,
            ..Default::default()
        }
    }
    /// Construct an empty texture coordinates array with the given dimensionality.
    pub fn tcoords(name: impl Into<String>, num_comp: u32) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::TCoords(num_comp),
            ..Default::default()
        }
    }
    /// Construct an empty generic array with the given number of components.
    pub fn new(name: impl Into<String>, num_comp: u32) -> Self {
        DataArray {
            name: name.into(),
            elem: ElementType::Generic(num_comp),
            ..Default::default()
        }
    }

    /// Get the number of components per element.
    ///
    /// This is equal to `self.len() / self.num_elem()`.
    pub fn num_comp(&self) -> usize {
        self.elem.num_comp() as usize
    }
}

impl FieldArray {
    /// Construct an empty field array with the given number of components.
    pub fn new(name: impl Into<String>, num_comp: u32) -> FieldArray {
        FieldArray {
            name: name.into(),
            elem: num_comp,
            data: IOBuffer::default(),
        }
    }

    /// Get the number of components per element.
    ///
    /// This is equal to `self.len() / self.num_elem()`.
    pub fn num_comp(&self) -> usize {
        self.elem as usize
    }
}

/// The type of element being represented inside a `DataArray`.
///
/// This is used to identify attribute types used by the Legacy VTK format, Additionally, this type
/// is used to tag active XML `DataArray`s as one of `Scalars`, `Vectors`, `Normals`, `Tensors`,
/// and `TCoords` as appropriate.
///
/// If an XML `DataArray` is marked tagged by any variant other than `Generic` (or Legacy only types
/// like `ColorScalars` and `LookupTable`) then it is considered active. If there is more than one
/// tagged attribute with the same type, then the first one is considered active.
#[derive(Clone, PartialEq, Debug)]
pub enum ElementType {
    /// Color Scalars represent floats in the range 0 to 1.
    ///
    /// The number of components per element is represented by the associated integer value.
    ///
    /// Identifies the `COLOR_SCALARS` legacy attribute. This is a legacy only type.
    ColorScalars(u32),
    /// A lookup table element is 4 color components: red, green, blue and alpha.
    ///
    /// Identifies the `LOOKUP_TABLE` legacy attribute. This is a legacy only type.
    LookupTable,
    /// A scalar field of 1, 2, 3 or 4 components.
    ///
    /// An associated lookup table can be specified corresponding to another attribute.
    ///
    /// Identifies the `SCALARS` legacy attribute.
    Scalars {
        /// Number of components per element.
        num_comp: u32,
        /// The name of an optional lookup table. Legacy only.
        lookup_table: Option<String>,
    },
    /// Vectors are triplets of x, y and z components.
    ///
    /// Identifies the `VECTORS` legacy attribute.
    Vectors,
    /// Normals are triplets of x, y and z components representing a normal vector.
    ///
    /// Normals are assumed to be unit length (normalized).
    ///
    /// Identifies the `NORMALS` legacy attribute.
    Normals,
    /// Texture coordinates can be 1, 2 or 3 dimensions.
    ///
    /// Identifies the `TEXTURE_COORDINATES` legacy attribute.
    TCoords(u32),
    /// Tensors are 3x3 matrices.
    ///
    /// These are given in full row major form:
    /// ```verbatim
    ///     t_00, t_01, t_02,
    ///     t_10, t_11, t_12,
    ///     t_20, t_21, t_22,
    /// ```
    /// Note that symmetry is assumed (`t_ij == t_ji`).
    ///
    /// Identifies the `TENSORS` legacy attribute.
    Tensors,
    /// Generic element with any number of components.
    ///
    /// This element type is used to identify fields in the Legacy format.
    Generic(u32),
}

impl Default for ElementType {
    fn default() -> ElementType {
        ElementType::Generic(1)
    }
}

impl ElementType {
    /// Get the number of components for this element as a 32 bit integer.
    pub fn num_comp(&self) -> u32 {
        match self {
            ElementType::ColorScalars(n) => *n,
            ElementType::LookupTable => 4,
            ElementType::Scalars { num_comp, .. } => *num_comp as u32,
            ElementType::Vectors | ElementType::Normals => 3,
            ElementType::TCoords(n) => *n as u32,
            ElementType::Tensors => 9,
            ElementType::Generic(n) => *n,
        }
    }
}

/// Data structure that stores a VTK attribute.
#[derive(Clone, PartialEq, Debug)]
pub enum Attribute {
    /// A data array with any number of components.
    ///
    /// This is the standard way to represent data in XML formats.
    ///
    /// It is also used to represent `VECTORS`, `NORMALS`, `TEXTURE_COORDINATES`, `LOOKUP_TABLE`s,
    /// `COLOR_SCALARS` and `TENSORS` in the legacy VTK format, each of which are identified by the
    /// `elem` field in the [`DataArray`] struct.
    ///
    /// [`DataArray`]: struct.DataArray.html
    DataArray(DataArray),
    /// Field attribute.
    ///
    /// Essentially an array of arrays of any size.
    /// This can be used to represent data for alternative topologies that don't correspond to the
    /// current data set, like UV coordinate topology with seams.
    ///
    /// This is a Legacy only attribute type.
    Field {
        name: String,
        data_array: Vec<FieldArray>,
    },
}

impl Attribute {
    /// Construct a new scalars attribute with an associated lookup table.
    pub fn scalars_with_lookup(
        name: impl Into<String>,
        num_comp: u32,
        lookup_table: impl Into<String>,
    ) -> Attribute {
        Attribute::DataArray(DataArray::scalars_with_lookup(name, num_comp, lookup_table))
    }
    /// Construct a new scalars attribute.
    pub fn scalars(name: impl Into<String>, num_comp: u32) -> Attribute {
        Attribute::DataArray(DataArray::scalars(name, num_comp))
    }
    /// Construct a new color scalars attribute.
    pub fn color_scalars(name: impl Into<String>, num_comp: u32) -> Attribute {
        Attribute::DataArray(DataArray::color_scalars(name, num_comp))
    }
    /// Construct a new lookup table attribute.
    pub fn lookup_table(name: impl Into<String>) -> Attribute {
        Attribute::DataArray(DataArray::lookup_table(name))
    }
    /// Construct a new vectors attribute.
    pub fn vectors(name: impl Into<String>) -> Attribute {
        Attribute::DataArray(DataArray::vectors(name))
    }
    /// Construct a new normals attribute.
    pub fn normals(name: impl Into<String>) -> Attribute {
        Attribute::DataArray(DataArray::normals(name))
    }
    /// Construct a new tensors attribute.
    pub fn tensors(name: impl Into<String>) -> Attribute {
        Attribute::DataArray(DataArray::tensors(name))
    }
    /// Construct a new texture coordinates attribute with the given dimensionality.
    pub fn tcoords(name: impl Into<String>, num_comp: u32) -> Attribute {
        Attribute::DataArray(DataArray::tcoords(name, num_comp))
    }
    /// Construct a new generic attribute with the given number of components.
    pub fn generic(name: impl Into<String>, num_comp: u32) -> Attribute {
        Attribute::DataArray(DataArray::new(name, num_comp))
    }
    /// Construct a new field attribute with the given name.
    pub fn field(name: impl Into<String>) -> Attribute {
        Attribute::Field {
            name: name.into(),
            data_array: Vec::new(),
        }
    }

    /// Set the data of this attribute to the given buffer.
    ///
    /// If this attribute is a `Field`, then nothing is changed.
    ///
    /// If the data was previously already set, it will be overwritten with the one given in this
    /// function.
    pub fn with_data(mut self, new_data: impl Into<IOBuffer>) -> Self {
        if let Attribute::DataArray(DataArray { data, .. }) = &mut self {
            *data = new_data.into();
        }
        self
    }

    /// Add a field array to the field attribute.
    ///
    /// If this attribute is not a `Field`, nothing is changed.
    ///
    /// # Examples
    ///
    /// One can collect a number of field arrays into a field attribute using with a sequence of
    /// calls to `add_field_data`.
    ///
    /// ```
    /// use vtkio::model::{Attribute, FieldArray};
    ///
    /// let field = Attribute::field("Data")
    ///     .add_field_data(FieldArray::new("A", 1))
    ///     .add_field_data(FieldArray::new("B", 2))
    ///     .add_field_data(FieldArray::new("C", 5));
    /// ```
    pub fn add_field_data(mut self, data: impl Into<FieldArray>) -> Self {
        if let Attribute::Field { data_array, .. } = &mut self {
            data_array.push(data.into());
        }
        self
    }
}

/// Point and cell attributes.
#[derive(Clone, PartialEq, Debug, Default)]
pub struct Attributes {
    pub point: Vec<Attribute>,
    pub cell: Vec<Attribute>,
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
    },
}

impl Default for VertexNumbers {
    fn default() -> VertexNumbers {
        VertexNumbers::XML {
            connectivity: Vec::new(),
            offsets: Vec::new(),
        }
    }
}

impl VertexNumbers {
    /// Returns the total number of vertices among all the cells.
    #[inline]
    pub fn num_verts(&self) -> usize {
        match self {
            VertexNumbers::Legacy {
                vertices,
                num_cells,
            } => vertices.len() - *num_cells as usize,
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
            VertexNumbers::Legacy {
                num_cells,
                vertices,
            } => (num_cells, vertices),
            VertexNumbers::XML {
                connectivity,
                offsets,
            } => {
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
    /// Converts `self` into `XML` format.
    ///
    /// Returns a connectivity and offsets array pair as in the `XML` variant.
    pub fn into_xml(self) -> (Vec<u32>, Vec<u32>) {
        match self {
            VertexNumbers::Legacy {
                num_cells,
                vertices,
            } => {
                let num_cells = usize::try_from(num_cells).unwrap();
                let num_verts = vertices.len();
                let mut connectivity = Vec::with_capacity(vertices.len() - num_cells);
                let mut offsets = Vec::with_capacity(num_cells);
                let mut n = -1i64;
                let mut prev_off = 0;
                for v in vertices {
                    if n > 0 {
                        connectivity.push(v);
                        n -= 1;
                    } else {
                        offsets.push(v + prev_off);
                        prev_off += v;
                        n = v.into();
                    }
                }
                assert_eq!(connectivity.len(), num_verts - num_cells);
                assert_eq!(offsets.len(), num_cells);
                (connectivity, offsets)
            }
            VertexNumbers::XML {
                connectivity,
                offsets,
            } => (connectivity, offsets),
        }
    }
}

/// Cells with variable types.
///
/// This struct corresponds to the `Cells` XML element or the CELLS and CELL_TYPES entries in the
/// legacy VTK format.
#[derive(Clone, PartialEq, Debug, Default)]
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
    pub fn num_verts(&self) -> usize {
        match self {
            PolyDataTopology::Vertices(vert_nums) => vert_nums.num_cells(),
            _ => 0,
        }
    }
    pub fn num_lines(&self) -> usize {
        match self {
            PolyDataTopology::Lines(vert_nums) => vert_nums.num_cells(),
            _ => 0,
        }
    }
    pub fn num_polys(&self) -> usize {
        match self {
            PolyDataTopology::Polygons(vert_nums) => vert_nums.num_cells(),
            _ => 0,
        }
    }
    pub fn num_strips(&self) -> usize {
        match self {
            PolyDataTopology::TriangleStrips(vert_nums) => vert_nums.num_cells(),
            _ => 0,
        }
    }
    pub fn num_cells(&self) -> usize {
        match self {
            PolyDataTopology::Vertices(vert_nums)
            | PolyDataTopology::Lines(vert_nums)
            | PolyDataTopology::Polygons(vert_nums)
            | PolyDataTopology::TriangleStrips(vert_nums) => vert_nums.num_cells(),
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
#[derive(Clone, Debug, PartialEq, Default)]
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
pub type RangeExtent = [RangeInclusive<i32>; 3];

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
                let dist = |x: RangeInclusive<i32>| (x.end() - x.start() + 1).max(0) as u32;
                [dist(x), dist(y), dist(z)]
            }
        }
    }

    /// Convert `Extent` to a triplet of ranges.
    ///
    /// If the extent is stored as `Extent::Dims` such as
    ///
    /// `[ nx, ny, nz ]`
    ///
    /// then the equivalent extent in XML format is returned:
    ///
    /// `[0..=nx, 0..=ny, 0..=nz]`
    pub fn into_ranges(self) -> [RangeInclusive<i32>; 3] {
        match self {
            Extent::Dims([nx, ny, nz]) => [0..=nx as i32, 0..=ny as i32, 0..=nz as i32],
            Extent::Ranges(rng) => rng,
        }
    }

    /// Compute the total number of points represented by this extent.
    pub fn num_points(&self) -> u32 {
        let [nx, ny, nz] = self.clone().into_dims();
        nx * ny * nz
    }

    /// Compute the total number of cells represented by this extent.
    pub fn num_cells(&self) -> u32 {
        let [nx, ny, nz] = self.clone().into_dims();
        (nx - 1) * (ny - 1) * (nz - 1)
    }
}

impl Default for Extent {
    /// The default extent is empty.
    fn default() -> Extent {
        Extent::Ranges([0..=0, 0..=0, 0..=0])
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
    Source(String, Option<Extent>),
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
            Piece::Source(path, _) => {
                let piece_vtk = crate::import(&path)?;
                let piece = Box::new(piece_vtk.data);
                self = Piece::Loaded(piece);
                self.load_into_piece_data()
            }
            Piece::Loaded(data_set) => match *data_set {
                DataSet::ImageData { pieces, .. }
                | DataSet::StructuredGrid { pieces, .. }
                | DataSet::RectilinearGrid { pieces, .. }
                | DataSet::UnstructuredGrid { pieces, .. }
                | DataSet::PolyData { pieces, .. } => pieces
                    .into_iter()
                    .next()
                    .ok_or(Error::MissingPieceData)?
                    .load_into_piece_data(),
                _ => return Err(Error::MissingPieceData),
            },
            Piece::Inline(piece_data) => Ok(*piece_data),
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
        /// A contiguous array of coordinates (x,y,z) representing the points in the mesh.
        points: IOBuffer,
        topo: Vec<PolyDataTopology>,
        data: Attributes,
    },
    /// UnstructuredGrid piece data.
    UnstructuredGrid {
        /// A contiguous array of coordinates (x,y,z) representing the points in the mesh.
        points: IOBuffer,
        cells: Cells,
        data: Attributes,
    },
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
        meta: Option<Box<MetaData>>,
        pieces: Vec<Piece>,
    },
    StructuredGrid {
        extent: Extent,
        meta: Option<Box<MetaData>>,
        pieces: Vec<Piece>,
    },
    RectilinearGrid {
        extent: Extent,
        meta: Option<Box<MetaData>>,
        pieces: Vec<Piece>,
    },
    /// 3D Unstructured grid. Note that `cells.num_cells` must equal `cell_types.len()`.
    UnstructuredGrid {
        meta: Option<Box<MetaData>>,
        pieces: Vec<Piece>,
    },
    /// 3D Polygon data.
    PolyData {
        meta: Option<Box<MetaData>>,
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
            PieceData::ImageData { extent, .. } => DataSet::ImageData {
                extent: extent.clone(),
                origin: [0.0; 3],
                spacing: [1.0; 3],
                meta: None,
                pieces: vec![Piece::Inline(Box::new(p))],
            },
            PieceData::StructuredGrid { extent, .. } => DataSet::StructuredGrid {
                extent: extent.clone(),
                meta: None,
                pieces: vec![Piece::Inline(Box::new(p))],
            },
            PieceData::RectilinearGrid { extent, .. } => DataSet::RectilinearGrid {
                extent: extent.clone(),
                meta: None,
                pieces: vec![Piece::Inline(Box::new(p))],
            },
            PieceData::UnstructuredGrid { .. } => DataSet::UnstructuredGrid {
                meta: None,
                pieces: vec![Piece::Inline(Box::new(p))],
            },
            PieceData::PolyData { .. } => DataSet::PolyData {
                meta: None,
                pieces: vec![Piece::Inline(Box::new(p))],
            },
        }
    }
}

/// A descriptor of the data set being stored.
///
/// This type is used to store the metadata of the data set for lazily loaded ("parallel") XML data
/// sets. This allows users to initialize the data pipeline before reading the data itself.
#[derive(Clone, PartialEq, Debug)]
pub enum MetaData {
    ImageData {
        ghost_level: u32,
        attributes: AttributesMetaData,
    },
    RectilinearGrid {
        ghost_level: u32,
        coords: [ScalarType; 3],
        attributes: AttributesMetaData,
    },
    StructuredGrid {
        ghost_level: u32,
        points_type: ScalarType,
        attributes: AttributesMetaData,
    },
    UnstructuredGrid {
        ghost_level: u32,
        points_type: ScalarType,
        attributes: AttributesMetaData,
    },
    PolyData {
        ghost_level: u32,
        points_type: ScalarType,
        attributes: AttributesMetaData,
    },
}

/// A descriptor of a collection of `Attribute`s.
///
/// This is used for lazy loading data sets in parallel XML files.
#[derive(Clone, PartialEq, Debug)]
pub struct AttributesMetaData {
    pub point_data: Vec<ArrayMetaData>,
    pub cell_data: Vec<ArrayMetaData>,
}

/// A descriptor of a `DataArray`.
///
/// This is used for lazy loading data sets in parallel XML files.
#[derive(Clone, PartialEq, Debug)]
pub struct ArrayMetaData {
    pub name: String,
    pub elem: ElementType,
    pub scalar_type: ScalarType,
}

/// Types of data that can be recognized by the parser. Not all data types are supported for all
/// classes.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum ScalarType {
    /// Data is interpreted as `u8` (unsigned 8 bit) chunks.
    Bit,
    /// Data is interpreted as `u8` (unsigned 8 bit) integers.
    U8,
    /// Data is interpreted as `i8` (signed 8 bit) integers.
    I8,
    /// Data is interpreted as `u16` (unsigned 16 bit) integers.
    U16,
    /// Data is interpreted as `i16` (signed 16 bit) integers.
    I16,
    /// Data is interpreted as `u32` (unsigned 32 bit) integers.
    U32,
    /// Data is interpreted as `i32` (signed 32 bit) integers.
    I32,
    /// Data is interpreted as `u64` (unsigned 64 bit) integers.
    U64,
    /// Data is interpreted as `i64` (signed 64 bit) integers.
    I64,
    /// Data is interpreted as `f32` (single precision) floats.
    F32,
    /// Data is interpreted as `f64` (double precision) floats.
    F64,
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ScalarType::Bit => write!(f, "bit"),
            ScalarType::U8 => write!(f, "unsigned_char"),
            ScalarType::I8 => write!(f, "char"),
            ScalarType::U16 => write!(f, "unsigned_short"),
            ScalarType::I16 => write!(f, "short"),
            ScalarType::U32 => write!(f, "unsigned_int"),
            ScalarType::I32 => write!(f, "int"),
            ScalarType::U64 => write!(f, "unsigned_long"),
            ScalarType::I64 => write!(f, "long"),
            ScalarType::F32 => write!(f, "float"),
            ScalarType::F64 => write!(f, "double"),
        }
    }
}

impl From<TypeId> for ScalarType {
    fn from(dt: TypeId) -> Self {
        match dt {
            x if x == TypeId::of::<u8>() => ScalarType::U8,
            x if x == TypeId::of::<i8>() => ScalarType::I8,
            x if x == TypeId::of::<u16>() => ScalarType::U16,
            x if x == TypeId::of::<i16>() => ScalarType::I16,
            x if x == TypeId::of::<u32>() => ScalarType::U32,
            x if x == TypeId::of::<i32>() => ScalarType::I32,
            x if x == TypeId::of::<u64>() => ScalarType::U64,
            x if x == TypeId::of::<i64>() => ScalarType::I64,
            x if x == TypeId::of::<f32>() => ScalarType::F32,
            x if x == TypeId::of::<f64>() => ScalarType::F64,
            _ => panic!("Specified type is unsupported by VTK."),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn io_buffer_iter() {
        let v = vec![1, 2, 3, 4];
        let buf = IOBuffer::U32(v);
        assert!(buf.iter::<u32>().is_some());
        assert!(buf.iter::<f32>().is_none());
    }

    #[test]
    fn io_buffer_into_vec() {
        let v = vec![1, 2, 3, 4];
        let buf = IOBuffer::U32(v.clone());
        assert!(buf.clone().into_vec::<f32>().is_none());
        assert_eq!(buf.into_vec::<u32>(), Some(v));
    }
}
