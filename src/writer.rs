use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};
use crate::model::*;
use std::fmt::Arguments;
use crate::IOBuffer;

mod write_vtk_impl {
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
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Header {
            Version,
            Title,
            /// Binary or ASCII.
            FileType,
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

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum DataSetError {
            FieldDataHeader,
            FieldArray(EntryPart),

            PolyData(DataSetPart),
            UnstructuredGrid(DataSetPart),
            StructuredGrid(DataSetPart),
            StructuredPoints(DataSetPart),
            RectilinearGrid(DataSetPart),
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

        /// Extract a raw IO Error from our error if any. This helps annotate the IO error with
        /// where it originated from when reported from lower level functions.
        impl Into<Option<std::io::ErrorKind>> for Error {
            fn into(self) -> Option<std::io::ErrorKind> {
                match self {
                    Error::IOError(err) => Some(err),
                    _ => None,
                }
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
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result;
        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result;

        fn write_attrib<BO: ByteOrder>(
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

        fn write_attrib_data<BO: ByteOrder>(&mut self, data: Vec<(String, Attribute)>) -> Result {
            for (name, attrib) in data {
                write!(self, "\n").map_err(|_| Error::NewLine)?;
                match attrib {
                    Attribute::Scalars {
                        num_comp,
                        lookup_table,
                        data,
                    } => {
                        write!(
                            self,
                            "SCALARS {} {} {}\n",
                            name,
                            DataType::from(data.element_type_id()),
                            num_comp
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::Scalars(EntryPart::Header))
                        })?;
                        write!(
                            self,
                            "LOOKUP_TABLE {}\n",
                            lookup_table.unwrap_or(String::from("default"))
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::Scalars(EntryPart::LookupTable))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::Scalars(EntryPart::Data(e.into())))
                        })?;
                    }
                    Attribute::ColorScalars { num_comp, data } => {
                        write!(self, "COLOR_SCALARS {} {}\n", name, num_comp).map_err(|_| {
                            Error::Attribute(AttributeError::ColorScalars(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::ColorScalars(EntryPart::Data(
                                e.into(),
                            )))
                        })?;
                    }
                    Attribute::LookupTable { data } => {
                        write!(self, "LOOKUP_TABLE {} {}\n", name, data.len() / 4).map_err(
                            |_| Error::Attribute(AttributeError::LookupTable(EntryPart::Header)),
                        )?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::LookupTable(EntryPart::Data(e.into())))
                        })?;
                    }
                    Attribute::Vectors { data } => {
                        write!(
                            self,
                            "VECTORS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::Vectors(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::Vectors(EntryPart::Data(e.into())))
                        })?;
                    }
                    Attribute::Normals { data } => {
                        write!(
                            self,
                            "NORMALS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::Normals(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::Normals(EntryPart::Data(e.into())))
                        })?;
                    }
                    Attribute::TextureCoordinates { dim, data } => {
                        write!(
                            self,
                            "TEXTURE_COORDINATES {} {} {}\n",
                            name,
                            dim,
                            DataType::from(data.element_type_id())
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::TextureCoordinates(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::TextureCoordinates(EntryPart::Data(
                                e.into(),
                            )))
                        })?;
                    }
                    Attribute::Tensors { data } => {
                        write!(
                            self,
                            "TENSORS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        )
                        .map_err(|_| {
                            Error::Attribute(AttributeError::Tensors(EntryPart::Header))
                        })?;
                        self.write_buf::<BO>(data).map_err(|e| {
                            Error::Attribute(AttributeError::Tensors(EntryPart::Data(e.into())))
                        })?;
                    }
                    Attribute::Field { data_array } => {
                        write!(self, "FIELD {} {}\n", name, data_array.len()).map_err(|_| {
                            Error::Attribute(AttributeError::Field(EntryPart::Header))
                        })?;
                        for field in data_array {
                            write!(
                                self,
                                "{} {} {} {}\n",
                                field.name,
                                field.num_comp,
                                field.data.len() / field.num_comp as usize,
                                DataType::from(field.data.element_type_id())
                            )
                            .map_err(|_| {
                                Error::Attribute(AttributeError::FieldArray(EntryPart::Header))
                            })?;
                            self.write_buf::<BO>(field.data).map_err(|e| {
                                Error::Attribute(AttributeError::FieldArray(EntryPart::Data(
                                    e.into(),
                                )))
                            })?;
                        }
                    }
                }
            }
            Ok(())
        }
        fn write_vtk_impl<BO: ByteOrder>(
            &mut self,
            vtk: Vtk,
        ) -> std::result::Result<&mut Self, Error> {
            write!(self, "# vtk DataFile Version {}\n", vtk.version)
                .map_err(|_| Error::Header(Header::Version))?;
            write!(self, "{}\n", vtk.title).map_err(|_| Error::Header(Header::Version))?;
            self.write_file_type()?;
            match vtk.data {
                DataSet::Field { name, data_array } => {
                    write!(self, "FIELD {} {}\n", name, data_array.len())
                        .map_err(|_| Error::DataSet(DataSetError::FieldDataHeader))?;
                    for field in data_array {
                        write!(
                            self,
                            "{} {} {} {}\n",
                            field.name,
                            field.num_comp,
                            field.data.len() / field.num_comp as usize,
                            DataType::from(field.data.element_type_id())
                        )
                        .map_err(|_| Error::DataSet(DataSetError::FieldArray(EntryPart::Header)))?;
                        self.write_buf::<BO>(field.data).map_err(|e| {
                            Error::DataSet(DataSetError::FieldArray(EntryPart::Data(e.into())))
                        })?;
                    }
                }

                DataSet::PolyData { points, topo, data } => {
                    write!(self, "DATASET POLYDATA\n")
                        .map_err(|_| Error::DataSet(DataSetError::PolyData(DataSetPart::Tags)))?;

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    )
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

                    write!(self, "\n").map_err(|_| Error::NewLine)?;

                    let mut num_cells = 0;
                    for topo_type in topo {
                        match topo_type {
                            PolyDataTopology::Vertices(_) => write!(self, "VERTICES"),
                            PolyDataTopology::Lines(_) => write!(self, "LINES"),
                            PolyDataTopology::Polygons(_) => write!(self, "POLYGONS"),
                            PolyDataTopology::TriangleStrips(_) => write!(self, "TRIANGLE_STRIPS"),
                        }
                        .map_err(|_| {
                            Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                EntryPart::Tags,
                            )))
                        })?;

                        let cells = match topo_type {
                            PolyDataTopology::Vertices(c) => c,
                            PolyDataTopology::Lines(c) => c,
                            PolyDataTopology::Polygons(c) => c,
                            PolyDataTopology::TriangleStrips(c) => c,
                        };

                        write!(self, " {} {}\n", cells.num_cells, cells.vertices.len()).map_err(
                            |_| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Sizes,
                                )))
                            },
                        )?;

                        self.write_u32_vec::<BO>(cells.vertices).map_err(|e| {
                            Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                EntryPart::Data(e.into()),
                            )))
                        })?;

                        num_cells += cells.num_cells as usize;
                    }

                    self.write_attrib::<BO>(data, num_points, num_cells)?;
                }

                DataSet::UnstructuredGrid {
                    points,
                    cells,
                    cell_types,
                    data,
                } => {
                    write!(self, "DATASET UNSTRUCTURED_GRID\n").map_err(|_| {
                        Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Tags))
                    })?;

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    )
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

                    write!(
                        self,
                        "\nCELLS {} {}\n",
                        cells.num_cells,
                        cells.vertices.len()
                    )
                    .map_err(|_| {
                        Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                            EntryPart::Header,
                        )))
                    })?;

                    self.write_u32_vec::<BO>(cells.vertices).map_err(|e| {
                        Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                            EntryPart::Data(e.into()),
                        )))
                    })?;

                    write!(self, "\nCELL_TYPES {}\n", cell_types.len()).map_err(|_| {
                        Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                            EntryPart::Header,
                        )))
                    })?;

                    self.write_cell_types::<BO>(cell_types)?;

                    self.write_attrib::<BO>(data, num_points, cells.num_cells as usize)?;
                }

                DataSet::StructuredPoints {
                    dims,
                    origin,
                    spacing,
                    data,
                } => {
                    write!(self, "DATASET STRUCTURED_POINTS\n").map_err(|_| {
                        Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Tags))
                    })?;

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2]).map_err(
                        |_| Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Dimensions)),
                    )?;

                    write!(self, "ORIGIN {} {} {}\n", origin[0], origin[1], origin[2]).map_err(
                        |_| Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Origin)),
                    )?;

                    if vtk.version.major < 2 {
                        write!(self, "ASPECT_RATIO")
                    } else {
                        write!(self, "SPACING")
                    }
                    .map_err(|_| {
                        Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Spacing(
                            EntryPart::Tags,
                        )))
                    })?;
                    write!(self, " {} {} {}\n", spacing[0], spacing[1], spacing[2]).map_err(
                        |_| {
                            Error::DataSet(DataSetError::StructuredPoints(DataSetPart::Spacing(
                                EntryPart::Sizes,
                            )))
                        },
                    )?;

                    let num_points = (dims[0] * dims[1] * dims[2]) as usize;
                    self.write_attrib::<BO>(data, num_points, 0)?;
                }

                DataSet::StructuredGrid { dims, points, data } => {
                    write!(self, "DATASET STRUCTURED_GRID\n").map_err(|_| {
                        Error::DataSet(DataSetError::StructuredGrid(DataSetPart::Tags))
                    })?;

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2]).map_err(
                        |_| Error::DataSet(DataSetError::StructuredGrid(DataSetPart::Dimensions)),
                    )?;

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    )
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
                    self.write_attrib::<BO>(data, num_points, 1)?;
                }

                DataSet::RectilinearGrid {
                    dims,
                    x_coords,
                    y_coords,
                    z_coords,
                    data,
                } => {
                    write!(self, "DATASET RECTILINEAR_GRID\n").map_err(|_| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::Tags))
                    })?;

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2]).map_err(
                        |_| Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::Dimensions)),
                    )?;

                    write!(
                        self,
                        "X_COORDINATES {} {}\n",
                        x_coords.len(),
                        DataType::from(x_coords.element_type_id())
                    )
                    .map_err(|_| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::XCoordinates(
                            EntryPart::Header,
                        )))
                    })?;
                    let num_x_coords = x_coords.len();
                    self.write_buf::<BO>(x_coords).map_err(|e| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::XCoordinates(
                            EntryPart::Data(e.into()),
                        )))
                    })?;
                    write!(
                        self,
                        "Y_COORDINATES {} {}\n",
                        y_coords.len(),
                        DataType::from(y_coords.element_type_id())
                    )
                    .map_err(|_| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::YCoordinates(
                            EntryPart::Header,
                        )))
                    })?;
                    let num_y_coords = y_coords.len();
                    self.write_buf::<BO>(y_coords).map_err(|e| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::YCoordinates(
                            EntryPart::Data(e.into()),
                        )))
                    })?;
                    write!(
                        self,
                        "Z_COORDINATES {} {}\n",
                        z_coords.len(),
                        DataType::from(z_coords.element_type_id())
                    )
                    .map_err(|_| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::ZCoordinates(
                            EntryPart::Header,
                        )))
                    })?;
                    let num_z_coords = z_coords.len();
                    self.write_buf::<BO>(z_coords).map_err(|e| {
                        Error::DataSet(DataSetError::RectilinearGrid(DataSetPart::ZCoordinates(
                            EntryPart::Data(e.into()),
                        )))
                    })?;

                    let num_points = num_x_coords * num_y_coords * num_z_coords;
                    let num_cells = (num_x_coords - 1) * (num_y_coords - 1) * (num_z_coords - 1);
                    self.write_attrib::<BO>(data, num_points, num_cells)?;
                }
            }

            write!(self, "\n").map_err(|_| Error::NewLine)?;
            Ok(self)
        }
    }
    impl WriteVtkImpl for Vec<u8> {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::io::Write::write_fmt(self, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            write!(self, "BINARY\n\n").map_err(|_| Error::Header(Header::FileType))
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err_fn = |ek: Option<std::io::ErrorKind>| {
                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                    EntryPart::Data(ek),
                )))
            };
            let err = |e: std::io::Error| err_fn(Some(e.kind()));
            for t in data {
                self.write_i32::<BO>(t as i32)
                    .map_err(err)?;
            }
            write!(self, "\n").map_err(|_| Error::NewLine)
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result {
            let buf = IOBuffer::from(data);
            self.write_buf::<BO>(buf)
        }
        fn write_buf<BO: ByteOrder>(&mut self, buf: IOBuffer) -> Result {
            use std::any::TypeId;

            fn write_buf_impl<T, W, E>(buf: IOBuffer, writer: &mut W, elem_writer: E) -> Result
            where
                W: WriteBytesExt,
                E: Fn(&mut W, T) -> std::io::Result<()>,
            {
                for elem in buf.reinterpret_into_vec::<T>() {
                    elem_writer(writer, elem)?;
                }
                Ok(())
            }

            match buf.element_type_id() {
                x if x == TypeId::of::<u8>() => write_buf_impl(buf, self, Self::write_u8)?,
                x if x == TypeId::of::<i8>() => write_buf_impl(buf, self, Self::write_i8)?,
                x if x == TypeId::of::<u16>() => {
                    write_buf_impl(buf, self, Self::write_u16::<BO>)?;
                }
                x if x == TypeId::of::<i16>() => {
                    write_buf_impl(buf, self, Self::write_i16::<BO>)?;
                }
                x if x == TypeId::of::<u32>() => {
                    write_buf_impl(buf, self, Self::write_u32::<BO>)?;
                }
                x if x == TypeId::of::<i32>() => {
                    write_buf_impl(buf, self, Self::write_i32::<BO>)?;
                }
                x if x == TypeId::of::<u64>() => {
                    write_buf_impl(buf, self, Self::write_u64::<BO>)?;
                }
                x if x == TypeId::of::<i64>() => {
                    write_buf_impl(buf, self, Self::write_i64::<BO>)?;
                }
                x if x == TypeId::of::<f32>() => {
                    write_buf_impl(buf, self, Self::write_f32::<BO>)?;
                }
                x if x == TypeId::of::<f64>() => {
                    write_buf_impl(buf, self, Self::write_f64::<BO>)?;
                }
                _ => {}
            }

            write!(self, "\n")
        }
    }

    impl WriteVtkImpl for String {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::fmt::Write::write_fmt(self, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            write!(self, "ASCII\n\n").map_err(|_| Error::Header(Header::FileType))
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err = Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                EntryPart::Data(None),
            )));
            for t in data {
                write!(self, "{}\n", t as u8).map_err(|_| err)?;
            }
            write!(self, "\n").map_err(|_| err)
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result {
            for i in 0..data.len() {
                write!(self, "{}", data[i])?;
                if i < data.len() - 1 {
                    // add an extra space between elements
                    write!(self, " ")?;
                }
            }
            write!(self, "\n") // finish with a new line
        }

        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result {
            write!(self, "{}\n", data)
        }
    }
}

pub use self::write_vtk_impl::Error;

pub trait WriteVtk: write_vtk_impl::WriteVtkImpl {
    fn write_vtk(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<NativeEndian>(vtk)
    }
    fn write_vtk_le(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<LittleEndian>(vtk)
    }
    fn write_vtk_be(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<BigEndian>(vtk)
    }
}

impl WriteVtk for Vec<u8> {}
impl WriteVtk for String {}
