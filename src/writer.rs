use std::fmt::Arguments;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use num_traits::ToPrimitive;

use crate::model::ByteOrder as ByteOrderTag;
use crate::model::*;

/// A `Write` wrapper for writing in ASCII format.
pub struct AsciiWriter<W: std::fmt::Write>(pub W);

/// A `Write` wrapper for writing in binary format.
pub struct BinaryWriter<W: std::io::Write>(pub W);

mod write_vtk_impl {
    use std::fmt::Display;

    use super::*;
    use byteorder::WriteBytesExt;

    pub mod error {
        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum EntryPart {
            /// The part of a header with just tags.
            Tags,
            /// The part of the header with corresponding size info.
            Sizes,
            /// Tags and sizes together.
            Header,
            /// The actually data for the entry (this can be binary or ASCII).
            /// If applicable, this enum will report any IO errors when writing data.
            Data(Option<std::io::ErrorKind>),
            /// Lookup table name. Only relevant for Scalars.
            LookupTable,
        }

        impl std::fmt::Display for EntryPart {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use EntryPart::*;
                match self {
                    Tags => write!(f, "Tags"),
                    Sizes => write!(f, "Sizes"),
                    Header => write!(f, "Header"),
                    Data(kind) => write!(f, "Data: {:?}", kind),
                    LookupTable => write!(f, "Lookup table"),
                }
            }
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum AttributeError {
            Scalars(EntryPart),
            ColorScalars(EntryPart),
            LookupTable(EntryPart),
            Vectors(EntryPart),
            Normals(EntryPart),
            TextureCoordinates(EntryPart),
            Tensors(EntryPart),
            Field(EntryPart),
            FieldArray(EntryPart),
            UnrecognizedAttributeType,
        }

        impl std::fmt::Display for AttributeError {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use AttributeError::*;
                match self {
                    Scalars(part) => write!(f, "Scalars: {}", part),
                    ColorScalars(part) => write!(f, "Color scalars: {}", part),
                    LookupTable(part) => write!(f, "Lookup table: {}", part),
                    Vectors(part) => write!(f, "Vectors: {}", part),
                    Normals(part) => write!(f, "Normals: {}", part),
                    TextureCoordinates(part) => write!(f, "Texture coordinates: {}", part),
                    Tensors(part) => write!(f, "Tensors: {}", part),
                    Field(part) => write!(f, "Field: {}", part),
                    FieldArray(part) => write!(f, "Field array: {}", part),
                    UnrecognizedAttributeType => write!(f, "Unrecognized attribute type"),
                }
            }
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Header {
            Version,
            Title,
            /// Binary or ASCII.
            FileType,
        }

        impl std::fmt::Display for Header {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    Header::Version => write!(f, "Version"),
                    Header::Title => write!(f, "Title"),
                    Header::FileType => write!(f, "File type (BINARY or ASCII)"),
                }
            }
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum DataSetPart {
            /// Tags identifying the data set type. For example UNSTRUCTURED_GRID or POLY_DATA.
            Tags,
            Points(EntryPart),
            Cells(EntryPart),
            CellTypes(EntryPart),
            Dimensions,
            Origin,
            Spacing(EntryPart),
            XCoordinates(EntryPart),
            YCoordinates(EntryPart),
            ZCoordinates(EntryPart),
        }

        impl std::fmt::Display for DataSetPart {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use DataSetPart::*;
                match self {
                    Tags => write!(f, "Tags"),
                    Points(part) => write!(f, "Points: {}", part),
                    Cells(part) => write!(f, "Cells: {}", part),
                    CellTypes(part) => write!(f, "Cell types: {}", part),
                    Dimensions => write!(f, "Dimensions"),
                    Origin => write!(f, "Origin"),
                    Spacing(part) => write!(f, "Spacing: {}", part),
                    XCoordinates(part) => write!(f, "X coords: {}", part),
                    YCoordinates(part) => write!(f, "Y coords: {}", part),
                    ZCoordinates(part) => write!(f, "Z coords: {}", part),
                }
            }
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum DataSetError {
            FieldDataHeader,
            FieldArray(EntryPart),

            PolyData(DataSetPart),
            UnstructuredGrid(DataSetPart),
            StructuredGrid(DataSetPart),
            StructuredPoints(DataSetPart),
            RectilinearGrid(DataSetPart),

            /// Piece data type doesn't match data set type.
            PieceDataMismatch,
            /// No piece data found for this data set.
            MissingPieceData,
        }

        impl std::fmt::Display for DataSetError {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use DataSetError::*;
                match self {
                    FieldDataHeader => write!(f, "Field data header"),
                    FieldArray(entry) => write!(f, "Field array: {}", entry),

                    PolyData(part) => write!(f, "Poly data: {}", part),
                    UnstructuredGrid(part) => write!(f, "Unstructured grid: {}", part),
                    StructuredGrid(part) => write!(f, "Structured grid: {}", part),
                    StructuredPoints(part) => write!(f, "Structured points: {}", part),
                    RectilinearGrid(part) => write!(f, "Rectilinear grid: {}", part),

                    PieceDataMismatch => write!(f, "Piece data mismatch"),
                    MissingPieceData => write!(f, "Missing piece data"),
                }
            }
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Error {
            PointDataHeader,
            CellDataHeader,
            Attribute(AttributeError),

            Header(Header),
            DataSet(DataSetError),
            NewLine,

            /// Generic formatting error originating from [`std::fmt::Error`].
            FormatError,
            /// Generic IO error originating from [`std::io::Error`].
            IOError(std::io::ErrorKind),
        }

        impl std::fmt::Display for Error {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match self {
                    Error::PointDataHeader => write!(f, "POINT_DATA header"),
                    Error::CellDataHeader => write!(f, "CELL_DATA header"),
                    Error::Attribute(attrib_err) => write!(f, "Attribute: {}", attrib_err),
                    Error::Header(header_err) => write!(f, "Header: {}", header_err),
                    Error::DataSet(data_set_err) => write!(f, "Data set: {}", data_set_err),
                    Error::NewLine => write!(f, "New line"),
                    Error::FormatError => write!(f, "Format error"),
                    Error::IOError(kind) => write!(f, "IO Error: {:?}", kind),
                }
            }
        }

        impl std::error::Error for Error {}

        /// Extract a raw IO Error from our error if any. This helps annotate the IO error with
        /// where it originated from when reported from lower level functions.
        impl From<Error> for Option<std::io::ErrorKind> {
            fn from(val: Error) -> Self {
                match val {
                    Error::IOError(err) => Some(err),
                    _ => None,
                }
            }
        }

        impl From<DataSetError> for Error {
            fn from(e: DataSetError) -> Error {
                Error::DataSet(e)
            }
        }

        impl From<std::fmt::Error> for Error {
            fn from(_: std::fmt::Error) -> Error {
                Error::FormatError
            }
        }

        impl From<std::io::Error> for Error {
            fn from(err: std::io::Error) -> Error {
                Error::IOError(err.kind())
            }
        }
    }

    pub use self::error::Error;
    use self::error::*;

    /// A typical result of a write operation.
    type Result = std::result::Result<(), Error>;

    pub trait WriteVtkImpl {
        /// This function is called by the `write!` macro used throughout this module.
        /// Each writer needs to call the appropriate `write_fmt` in the implementation
        /// of this method.
        fn write_fmt(&mut self, args: Arguments) -> Result;
        fn write_file_type(&mut self) -> Result;
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result;
        fn write_vec<T: Display + ToPrimitive + 'static, BO: ByteOrder>(
            &mut self,
            data: Vec<T>,
        ) -> Result;
        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result;
        fn write_color_scalars_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            self.write_buf::<BO>(buf)
        }

        fn write_attributes<BO: ByteOrder>(
            &mut self,
            data: Attributes,
            num_points: usize,
            num_cells: usize,
        ) -> Result {
            write!(self, "\nPOINT_DATA {}\n", num_points).map_err(|_| Error::PointDataHeader)?;
            self.write_attrib_data::<BO>(data.point)?;

            write!(self, "\nCELL_DATA {}\n", num_cells).map_err(|_| Error::CellDataHeader)?;
            self.write_attrib_data::<BO>(data.cell)
        }

        fn write_attrib<BO: ByteOrder>(&mut self, attrib: Attribute) -> Result {
            // Auxiliary generic attributes that cannot be easily mapped to a standard Legacy type.
            // These are later written into a separate auxiliary field.
            let mut auxiliary = Vec::new();
            match attrib {
                Attribute::DataArray(DataArray { name, elem, data }) => {
                    match elem {
                        ElementType::Scalars {
                            num_comp,
                            lookup_table,
                        } => {
                            writeln!(self, "SCALARS {} {} {}", name, data.scalar_type(), num_comp)
                                .map_err(|_| {
                                    Error::Attribute(AttributeError::Scalars(EntryPart::Header))
                                })?;
                            writeln!(
                                self,
                                "LOOKUP_TABLE {}",
                                lookup_table.unwrap_or_else(|| String::from("default"))
                            )
                            .map_err(|_| {
                                Error::Attribute(AttributeError::Scalars(EntryPart::LookupTable))
                            })?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::Scalars(EntryPart::Data(e.into())))
                            })?;
                        }
                        ElementType::ColorScalars(num_comp) => {
                            writeln!(self, "COLOR_SCALARS {} {}", name, num_comp).map_err(
                                |_| {
                                    Error::Attribute(AttributeError::ColorScalars(
                                        EntryPart::Header,
                                    ))
                                },
                            )?;
                            self.write_color_scalars_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::ColorScalars(EntryPart::Data(
                                    e.into(),
                                )))
                            })?;
                        }
                        ElementType::LookupTable => {
                            writeln!(self, "LOOKUP_TABLE {} {}", name, data.len() / 4).map_err(
                                |_| {
                                    Error::Attribute(AttributeError::LookupTable(EntryPart::Header))
                                },
                            )?;
                            self.write_color_scalars_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::LookupTable(EntryPart::Data(
                                    e.into(),
                                )))
                            })?;
                        }
                        ElementType::Vectors => {
                            writeln!(self, "VECTORS {} {}", name, data.scalar_type()).map_err(
                                |_| Error::Attribute(AttributeError::Vectors(EntryPart::Header)),
                            )?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::Vectors(EntryPart::Data(e.into())))
                            })?;
                        }
                        ElementType::Normals => {
                            writeln!(self, "NORMALS {} {}", name, data.scalar_type()).map_err(
                                |_| Error::Attribute(AttributeError::Normals(EntryPart::Header)),
                            )?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::Normals(EntryPart::Data(e.into())))
                            })?;
                        }
                        ElementType::TCoords(dim) => {
                            writeln!(
                                self,
                                "TEXTURE_COORDINATES {} {} {}",
                                name,
                                dim,
                                data.scalar_type()
                            )
                            .map_err(|_| {
                                Error::Attribute(AttributeError::TextureCoordinates(
                                    EntryPart::Header,
                                ))
                            })?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::TextureCoordinates(
                                    EntryPart::Data(e.into()),
                                ))
                            })?;
                        }
                        ElementType::Tensors => {
                            writeln!(self, "TENSORS {} {}", name, data.scalar_type()).map_err(
                                |_| Error::Attribute(AttributeError::Tensors(EntryPart::Header)),
                            )?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::Tensors(EntryPart::Data(e.into())))
                            })?;
                        }
                        ElementType::Generic(n) => {
                            // Try to convert into an element type representable in Legacy format.
                            match n {
                                3 => self.write_attrib::<BO>(Attribute::DataArray(DataArray {
                                    name,
                                    elem: ElementType::Vectors,
                                    data,
                                }))?,
                                1 | 2 | 4 => {
                                    self.write_attrib::<BO>(Attribute::DataArray(DataArray {
                                        name,
                                        elem: ElementType::Scalars {
                                            num_comp: n,
                                            lookup_table: None,
                                        },
                                        data,
                                    }))?;
                                }
                                // TODO: A more sophisticated scheme could check the values to
                                // determine if the attribute is a Tensor for 9 component elements.
                                n => {
                                    auxiliary.push(FieldArray {
                                        name,
                                        elem: n,
                                        data,
                                    });
                                }
                            }
                        }
                    }
                }
                Attribute::Field { name, data_array } => {
                    writeln!(self, "FIELD {} {}", name, data_array.len())
                        .map_err(|_| Error::Attribute(AttributeError::Field(EntryPart::Header)))?;
                    for FieldArray {
                        name,
                        elem: num_comp,
                        data,
                    } in data_array
                    {
                        writeln!(
                            self,
                            "{} {} {} {}",
                            name,
                            num_comp,
                            data.len() / num_comp as usize,
                            data.scalar_type()
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::FieldArray(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::FieldArray(EntryPart::Data(e.into())))
                        })?;
                    }
                }
            }

            if !auxiliary.is_empty() {
                self.write_attrib::<BO>(Attribute::Field {
                    name: String::from("vtkio_auxiliary"),
                    data_array: auxiliary,
                })?;
            }
            Ok(())
        }

        fn write_attrib_data<BO: ByteOrder>(&mut self, attribs: Vec<Attribute>) -> Result {
            for attrib in attribs {
                writeln!(self).map_err(|_| Error::NewLine)?;
                self.write_attrib::<BO>(attrib)?;
            }
            Ok(())
        }
        fn write_vtk_impl<BO: ByteOrder>(
            &mut self,
            vtk: Vtk,
        ) -> std::result::Result<&mut Self, Error> {
            let version = vtk.version.to_legacy();
            let source_path = vtk.file_path.as_ref().map(|p| p.as_ref());
            writeln!(self, "# vtk DataFile Version {}.{}", version.0, version.1)
                .map_err(|_| Error::Header(Header::Version))?;
            writeln!(self, "{}", vtk.title).map_err(|_| Error::Header(Header::Version))?;
            self.write_file_type()?;
            match vtk.data {
                DataSet::Field { name, data_array } => {
                    writeln!(self, "FIELD {} {}", name, data_array.len())
                        .map_err(|_| Error::DataSet(DataSetError::FieldDataHeader))?;
                    for FieldArray {
                        name,
                        elem: num_comp,
                        data,
                    } in data_array
                    {
                        writeln!(
                            self,
                            "{} {} {} {}",
                            name,
                            num_comp,
                            data.len() / num_comp as usize,
                            data.scalar_type()
                        )
                        .map_err(|_| Error::DataSet(DataSetError::FieldArray(EntryPart::Header)))?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::DataSet(DataSetError::FieldArray(EntryPart::Data(e.into())))
                        })?;
                    }
                }

                DataSet::PolyData { pieces, .. } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(PolyDataPiece {
                        points,
                        verts,
                        lines,
                        polys,
                        strips,
                        data,
                    }) = piece.into_loaded_piece_data(source_path)
                    {
                        writeln!(self, "DATASET POLYDATA").map_err(|_| {
                            Error::DataSet(DataSetError::PolyData(DataSetPart::Tags))
                        })?;

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.scalar_type())
                            .map_err(|_| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Points(
                                    EntryPart::Header,
                                )))
                            })?;
                        let num_points = points.len() / 3;
                        self.write_buf::<BO>(points).map_err(|e| {
                            Error::DataSet(DataSetError::PolyData(DataSetPart::Points(
                                EntryPart::Data(e.into()),
                            )))
                        })?;

                        writeln!(self).map_err(|_| Error::NewLine)?;

                        let mut num_cells = 0;
                        let mut write_topo = |cell_verts: VertexNumbers, title: &str| -> Result {
                            write!(self, "{}", title).map_err(|_| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Tags,
                                )))
                            })?;

                            let cur_num_cells = cell_verts.num_cells();

                            writeln!(
                                self,
                                " {} {}",
                                cur_num_cells,
                                cur_num_cells + cell_verts.num_verts()
                            )
                            .map_err(|_| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Sizes,
                                )))
                            })?;

                            let (_, vertices) = cell_verts.into_legacy();

                            self.write_vec::<u32, BO>(vertices).map_err(|e| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Data(e.into()),
                                )))
                            })?;

                            num_cells += cur_num_cells;
                            Ok(())
                        };

                        verts
                            .map(|verts| write_topo(verts, "VERTICES"))
                            .transpose()?;
                        lines.map(|verts| write_topo(verts, "LINES")).transpose()?;
                        polys
                            .map(|verts| write_topo(verts, "POLYGONS"))
                            .transpose()?;
                        strips
                            .map(|verts| write_topo(verts, "TRIANGLE_STRIPS"))
                            .transpose()?;

                        self.write_attributes::<BO>(data, num_points, num_cells)?;
                    }
                }

                DataSet::UnstructuredGrid { pieces, .. } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(UnstructuredGridPiece {
                        points,
                        cells,
                        data,
                    }) = piece.into_loaded_piece_data(source_path)
                    {
                        writeln!(self, "DATASET UNSTRUCTURED_GRID").map_err(|_| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Tags))
                        })?;

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.scalar_type())
                            .map_err(|_| {
                                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Points(
                                    EntryPart::Header,
                                )))
                            })?;
                        let num_points = points.len() / 3;
                        self.write_buf::<BO>(points).map_err(|e| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Points(
                                EntryPart::Data(e.into()),
                            )))
                        })?;

                        let num_cells = cells.cell_verts.num_cells();

                        // Write CELLS structure.
                        if version.0 >= 5 {
                            // From version 5 and on the cells are written as an offsets and connectivity pair.
                            let (connectivity, offsets) = cells.cell_verts.into_xml();

                            writeln!(self, "\nCELLS {} {}", offsets.len(), connectivity.len())
                                .map_err(|_| {
                                    Error::DataSet(DataSetError::UnstructuredGrid(
                                        DataSetPart::Cells(EntryPart::Header),
                                    ))
                                })?;

                            writeln!(self, "\nOFFSETS vtktypeint64")?;
                            self.write_vec::<_, BO>(offsets).map_err(|e| {
                                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                                    EntryPart::Data(e.into()),
                                )))
                            })?;

                            writeln!(self, "\nCONNECTIVITY vtktypeint64")?;
                            self.write_vec::<_, BO>(connectivity).map_err(|e| {
                                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                                    EntryPart::Data(e.into()),
                                )))
                            })?;
                        } else {
                            let num_verts = cells.cell_verts.num_verts();

                            writeln!(self, "\nCELLS {} {}", num_cells, num_cells + num_verts)
                                .map_err(|_| {
                                    Error::DataSet(DataSetError::UnstructuredGrid(
                                        DataSetPart::Cells(EntryPart::Header),
                                    ))
                                })?;

                            let (_, vertices) = cells.cell_verts.into_legacy();

                            self.write_vec::<u32, BO>(vertices).map_err(|e| {
                                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                                    EntryPart::Data(e.into()),
                                )))
                            })?;
                        }

                        writeln!(self, "\nCELL_TYPES {}", cells.types.len()).map_err(|_| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                                EntryPart::Header,
                            )))
                        })?;

                        self.write_cell_types::<BO>(cells.types)?;

                        self.write_attributes::<BO>(data, num_points, num_cells)?;
                    }
                }

                DataSet::ImageData {
                    extent,
                    origin,
                    spacing,
                    pieces,
                    ..
                } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(ImageDataPiece { data, .. }) =
                        piece.into_loaded_piece_data(source_path)
                    {
                        writeln!(self, "DATASET STRUCTURED_POINTS").map_err(|_| {
                            Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Tags))
                        })?;

                        let dims = extent.into_dims();

                        writeln!(self, "DIMENSIONS {} {} {}", dims[0], dims[1], dims[2]).map_err(
                            |_| {
                                Error::DataSet(DataSetError::StructuredPoints(
                                    DataSetPart::Dimensions,
                                ))
                            },
                        )?;

                        writeln!(self, "ORIGIN {} {} {}", origin[0], origin[1], origin[2])
                            .map_err(|_| {
                                Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Origin))
                            })?;

                        if version.0 < 2 {
                            write!(self, "ASPECT_RATIO")
                        } else {
                            write!(self, "SPACING")
                        }
                        .map_err(|_| {
                            Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Spacing(
                                EntryPart::Tags,
                            )))
                        })?;
                        writeln!(self, " {} {} {}", spacing[0], spacing[1], spacing[2]).map_err(
                            |_| {
                                Error::DataSet(DataSetError::StructuredPoints(
                                    DataSetPart::Spacing(EntryPart::Sizes),
                                ))
                            },
                        )?;

                        let num_points = (dims[0] * dims[1] * dims[2]) as usize;
                        self.write_attributes::<BO>(data, num_points, 0)?;
                    }
                }

                DataSet::StructuredGrid { extent, pieces, .. } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(StructuredGridPiece { points, data, .. }) =
                        piece.into_loaded_piece_data(source_path)
                    {
                        writeln!(self, "DATASET STRUCTURED_GRID").map_err(|_| {
                            Error::DataSet(DataSetError::StructuredGrid(DataSetPart::Tags))
                        })?;

                        let dims = extent.into_dims();

                        writeln!(self, "DIMENSIONS {} {} {}", dims[0], dims[1], dims[2]).map_err(
                            |_| {
                                Error::DataSet(DataSetError::StructuredGrid(
                                    DataSetPart::Dimensions,
                                ))
                            },
                        )?;

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.scalar_type())
                            .map_err(|_| {
                                Error::DataSet(DataSetError::StructuredGrid(DataSetPart::Points(
                                    EntryPart::Header,
                                )))
                            })?;
                        let num_points = points.len() / 3;
                        self.write_buf::<BO>(points).map_err(|e| {
                            Error::DataSet(DataSetError::StructuredGrid(DataSetPart::Points(
                                EntryPart::Data(e.into()),
                            )))
                        })?;

                        assert_eq!((dims[0] * dims[1] * dims[2]) as usize, num_points);
                        self.write_attributes::<BO>(data, num_points, 1)?;
                    }
                }

                DataSet::RectilinearGrid { extent, pieces, .. } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(RectilinearGridPiece { coords, data, .. }) =
                        piece.into_loaded_piece_data(source_path)
                    {
                        writeln!(self, "DATASET RECTILINEAR_GRID").map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::Tags))
                        })?;

                        let dims = extent.into_dims();

                        writeln!(self, "DIMENSIONS {} {} {}", dims[0], dims[1], dims[2]).map_err(
                            |_| {
                                Error::DataSet(DataSetError::RectilinearGrid(
                                    DataSetPart::Dimensions,
                                ))
                            },
                        )?;

                        writeln!(
                            self,
                            "X_COORDINATES {} {}",
                            coords.x.len(),
                            coords.x.scalar_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::XCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_x_coords = coords.x.len();
                        self.write_buf::<BO>(coords.x).map_err(|e| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::XCoordinates(EntryPart::Data(e.into())),
                            ))
                        })?;
                        writeln!(
                            self,
                            "Y_COORDINATES {} {}",
                            coords.y.len(),
                            coords.y.scalar_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::YCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_y_coords = coords.y.len();
                        self.write_buf::<BO>(coords.y).map_err(|e| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::YCoordinates(EntryPart::Data(e.into())),
                            ))
                        })?;
                        writeln!(
                            self,
                            "Z_COORDINATES {} {}",
                            coords.z.len(),
                            coords.z.scalar_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::ZCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_z_coords = coords.z.len();
                        self.write_buf::<BO>(coords.z).map_err(|e| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::ZCoordinates(EntryPart::Data(e.into())),
                            ))
                        })?;

                        let num_points = num_x_coords * num_y_coords * num_z_coords;
                        let num_cells =
                            (num_x_coords - 1) * (num_y_coords - 1) * (num_z_coords - 1);
                        self.write_attributes::<BO>(data, num_points, num_cells)?;
                    }
                }
            }

            writeln!(self).map_err(|_| Error::NewLine)?;
            Ok(self)
        }
    }

    impl<W: std::io::Write> WriteVtkImpl for BinaryWriter<W> {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::io::Write::write_fmt(&mut self.0, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            writeln!(&mut self.0, "BINARY\n").map_err(|_| Error::Header(Header::FileType))
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err_fn = |ek: Option<std::io::ErrorKind>| {
                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                    EntryPart::Data(ek),
                )))
            };
            let err = |e: std::io::Error| err_fn(Some(e.kind()));
            for t in data {
                self.0.write_i32::<BO>(t as i32).map_err(err)?;
            }
            writeln!(&mut self.0).map_err(|_| Error::NewLine)
        }
        fn write_vec<T: Display + ToPrimitive + 'static, BO: ByteOrder>(
            &mut self,
            data: Vec<T>,
        ) -> Result {
            let buf = IOBuffer::from(data);
            self.write_buf::<BO>(buf)
        }
        fn write_color_scalars_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            fn write_buf_impl<T, W, F>(vec: Vec<T>, writer: &mut W, convert_to_u8: F) -> Result
            where
                W: WriteBytesExt,
                F: Fn(T) -> u8,
            {
                for elem in vec {
                    W::write_u8(writer, convert_to_u8(elem))?;
                }
                Ok(())
            }
            fn convert_float<T: Into<f64>>(input: T) -> u8 {
                (input.into() * 255.0).round().clamp(0.0, 255.0) as u8
            }

            match buf {
                IOBuffer::Bit(v) => write_buf_impl(v, &mut self.0, |x| x)?,
                IOBuffer::U8(v) => write_buf_impl(v, &mut self.0, |x| x)?,
                IOBuffer::I8(v) => write_buf_impl(v, &mut self.0, |x| x as u8)?,
                IOBuffer::U16(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::I16(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::U32(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::I32(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::U64(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::I64(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as u8)?;
                }
                IOBuffer::F32(v) => {
                    write_buf_impl(v, &mut self.0, convert_float)?;
                }
                IOBuffer::F64(v) => {
                    write_buf_impl(v, &mut self.0, convert_float)?;
                }
            }

            writeln!(&mut self.0)?;
            Ok(())
        }
        fn write_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            fn write_buf_impl<T, W, E>(vec: Vec<T>, writer: &mut W, elem_writer: E) -> Result
            where
                W: WriteBytesExt,
                E: Fn(&mut W, T) -> std::io::Result<()>,
            {
                for elem in vec {
                    elem_writer(writer, elem)?;
                }
                Ok(())
            }

            match buf {
                IOBuffer::Bit(v) => write_buf_impl(v, &mut self.0, W::write_u8)?,
                IOBuffer::U8(v) => write_buf_impl(v, &mut self.0, W::write_u8)?,
                IOBuffer::I8(v) => write_buf_impl(v, &mut self.0, W::write_i8)?,
                IOBuffer::U16(v) => {
                    write_buf_impl(v, &mut self.0, W::write_u16::<BO>)?;
                }
                IOBuffer::I16(v) => {
                    write_buf_impl(v, &mut self.0, W::write_i16::<BO>)?;
                }
                IOBuffer::U32(v) => {
                    write_buf_impl(v, &mut self.0, W::write_u32::<BO>)?;
                }
                IOBuffer::I32(v) => {
                    write_buf_impl(v, &mut self.0, W::write_i32::<BO>)?;
                }
                IOBuffer::U64(v) => {
                    write_buf_impl(v, &mut self.0, W::write_u64::<BO>)?;
                }
                IOBuffer::I64(v) => {
                    write_buf_impl(v, &mut self.0, W::write_i64::<BO>)?;
                }
                IOBuffer::F32(v) => {
                    write_buf_impl(v, &mut self.0, W::write_f32::<BO>)?;
                }
                IOBuffer::F64(v) => {
                    write_buf_impl(v, &mut self.0, W::write_f64::<BO>)?;
                }
            }

            writeln!(&mut self.0)?;
            Ok(())
        }
    }

    impl WriteVtkImpl for Vec<u8> {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            BinaryWriter(self).write_fmt(args)
        }
        fn write_file_type(&mut self) -> Result {
            BinaryWriter(self).write_file_type()
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            BinaryWriter(self).write_cell_types::<BO>(data)
        }
        fn write_vec<T: Display + ToPrimitive + 'static, BO: ByteOrder>(
            &mut self,
            data: Vec<T>,
        ) -> Result {
            BinaryWriter(self).write_vec::<T, BO>(data)
        }
        fn write_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            BinaryWriter(self).write_buf::<BO>(buf)
        }
        fn write_color_scalars_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            BinaryWriter(self).write_color_scalars_buf::<BO>(buf)
        }
    }

    impl<W: std::fmt::Write> WriteVtkImpl for AsciiWriter<W> {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::fmt::Write::write_fmt(&mut self.0, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            writeln!(&mut self.0, "ASCII\n").map_err(|_| Error::Header(Header::FileType))?;
            Ok(())
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err = Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                EntryPart::Data(None),
            )));
            for t in data {
                writeln!(&mut self.0, "{}", t as u8).map_err(|_| err)?;
            }
            writeln!(&mut self.0).map_err(|_| err)?;
            Ok(())
        }
        fn write_vec<T: Display + ToPrimitive + 'static, BO: ByteOrder>(
            &mut self,
            data: Vec<T>,
        ) -> Result {
            for i in 0..data.len() {
                write!(&mut self.0, "{}", data[i])?;
                if i < data.len() - 1 {
                    // add an extra space between elements
                    write!(&mut self.0, " ")?;
                }
            }
            writeln!(&mut self.0)?; // finish with a new line
            Ok(())
        }

        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result {
            writeln!(&mut self.0, "{}", data)?;
            Ok(())
        }
        fn write_color_scalars_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result {
            fn write_buf_impl<T, W, F>(vec: Vec<T>, writer: &mut W, convert_to_float: F) -> Result
            where
                W: std::fmt::Write,
                F: Fn(T) -> f32,
            {
                let mut iter = vec.into_iter().peekable();
                while let Some(elem) = iter.next() {
                    write!(writer, "{}", convert_to_float(elem))?;
                    if iter.peek().is_some() {
                        write!(writer, " ")?;
                    }
                }
                Ok(())
            }
            fn convert_int<T: Into<i64>>(input: T) -> f32 {
                0.max(input.into()) as f32 / 255.0
            }

            match data {
                IOBuffer::Bit(v) => write_buf_impl(v, &mut self.0, convert_int)?,
                IOBuffer::U8(v) => write_buf_impl(v, &mut self.0, convert_int)?,
                IOBuffer::I8(v) => write_buf_impl(v, &mut self.0, convert_int)?,
                IOBuffer::U16(v) => {
                    write_buf_impl(v, &mut self.0, convert_int)?;
                }
                IOBuffer::I16(v) => {
                    write_buf_impl(v, &mut self.0, convert_int)?;
                }
                IOBuffer::U32(v) => {
                    write_buf_impl(v, &mut self.0, convert_int)?;
                }
                IOBuffer::I32(v) => {
                    write_buf_impl(v, &mut self.0, convert_int)?;
                }
                IOBuffer::U64(v) => {
                    write_buf_impl(v, &mut self.0, |x| 0.max(x) as f32 / 255.0)?;
                }
                IOBuffer::I64(v) => {
                    write_buf_impl(v, &mut self.0, convert_int)?;
                }
                IOBuffer::F32(v) => write_buf_impl(v, &mut self.0, |x| x)?,
                IOBuffer::F64(v) => {
                    write_buf_impl(v, &mut self.0, |x| x as f32)?;
                }
            }

            writeln!(&mut self.0)?;
            Ok(())
        }
    }

    impl WriteVtkImpl for String {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            AsciiWriter(self).write_fmt(args)
        }
        fn write_file_type(&mut self) -> Result {
            AsciiWriter(self).write_file_type()
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            AsciiWriter(self).write_cell_types::<BO>(data)
        }
        fn write_vec<T: Display + ToPrimitive + 'static, BO: ByteOrder>(
            &mut self,
            data: Vec<T>,
        ) -> Result {
            AsciiWriter(self).write_vec::<T, BO>(data)
        }
        fn write_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            AsciiWriter(self).write_buf::<BO>(buf)
        }

        fn write_color_scalars_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            AsciiWriter(self).write_color_scalars_buf::<BO>(buf)
        }
    }
}

pub use self::write_vtk_impl::Error;

pub trait WriteVtk: write_vtk_impl::WriteVtkImpl {
    fn write_vtk(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        match vtk.byte_order {
            ByteOrderTag::LittleEndian => self.write_vtk_impl::<LittleEndian>(vtk),
            ByteOrderTag::BigEndian => self.write_vtk_impl::<BigEndian>(vtk),
        }
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in little endian format.
    fn write_vtk_le(&mut self, mut vtk: Vtk) -> Result<&mut Self, Error> {
        // Make sure the written file is consistent
        vtk.byte_order = ByteOrderTag::LittleEndian;
        self.write_vtk_impl::<LittleEndian>(vtk)
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in big endian format.
    fn write_vtk_be(&mut self, mut vtk: Vtk) -> Result<&mut Self, Error> {
        // Make sure the written file is consistent
        vtk.byte_order = ByteOrderTag::BigEndian;
        self.write_vtk_impl::<BigEndian>(vtk)
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in native endian format.
    #[cfg(target_endian = "little")]
    fn write_vtk_ne(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_le(vtk)
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in native endian format.
    #[cfg(target_endian = "big")]
    fn write_vtk_ne(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_be(vtk)
    }
}

impl<W: std::fmt::Write> WriteVtk for AsciiWriter<W> {}
impl<W: std::io::Write> WriteVtk for BinaryWriter<W> {}
impl WriteVtk for String {}
impl WriteVtk for Vec<u8> {}
