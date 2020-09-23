use std::fmt::Arguments;

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};

use crate::model::ByteOrder as ByteOrderTag;
use crate::model::*;

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
            UnrecognizedAttributeType,
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

            /// Piece data type doesn't match data set type.
            PieceDataMismatch,
            /// No piece data found for this data set.
            MissingPieceData,
        }

        #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Error {
            PointDataHeader,
            CellDataHeader,
            Attribute(AttributeError),

            Header(Header),
            DataSet(DataSetError),
            NewLine,

            /// Unexpected type stored in referenced data buffer. This is most likely caused by
            /// data corruption.
            DataMismatchError,
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
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result;
        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result;

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
                            writeln!(
                                self,
                                "SCALARS {} {} {}",
                                name,
                                DataType::from(data.data_type()),
                                num_comp
                            )
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
                            self.write_buf::<BO>(data).map_err(|e| {
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
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::LookupTable(EntryPart::Data(
                                    e.into(),
                                )))
                            })?;
                        }
                        ElementType::Vectors => {
                            writeln!(self, "VECTORS {} {}", name, data.data_type()).map_err(
                                |_| Error::Attribute(AttributeError::Vectors(EntryPart::Header)),
                            )?;
                            self.write_buf::<BO>(data).map_err(|e| {
                                Error::Attribute(AttributeError::Vectors(EntryPart::Data(e.into())))
                            })?;
                        }
                        ElementType::Normals => {
                            writeln!(self, "NORMALS {} {}", name, data.data_type()).map_err(
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
                                data.data_type()
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
                            writeln!(self, "TENSORS {} {}", name, data.data_type()).map_err(
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
                            data.data_type()
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
            writeln!(self, "# vtk DataFile Version {}", vtk.version)
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
                            data.data_type()
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
                    if let Ok(PieceData::PolyData { points, topo, data }) =
                        piece.load_into_piece_data()
                    {
                        writeln!(self, "DATASET POLYDATA").map_err(|_| {
                            Error::DataSet(DataSetError::PolyData(DataSetPart::Tags))
                        })?;

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.data_type())
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
                        for topo_type in topo {
                            match topo_type {
                                PolyDataTopology::Vertices(_) => write!(self, "VERTICES"),
                                PolyDataTopology::Lines(_) => write!(self, "LINES"),
                                PolyDataTopology::Polygons(_) => write!(self, "POLYGONS"),
                                PolyDataTopology::TriangleStrips(_) => {
                                    write!(self, "TRIANGLE_STRIPS")
                                }
                            }
                            .map_err(|_| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Tags,
                                )))
                            })?;

                            let cell_verts = match topo_type {
                                PolyDataTopology::Vertices(c) => c,
                                PolyDataTopology::Lines(c) => c,
                                PolyDataTopology::Polygons(c) => c,
                                PolyDataTopology::TriangleStrips(c) => c,
                            };

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

                            self.write_u32_vec::<BO>(vertices).map_err(|e| {
                                Error::DataSet(DataSetError::PolyData(DataSetPart::Cells(
                                    EntryPart::Data(e.into()),
                                )))
                            })?;

                            num_cells += cur_num_cells as usize;
                        }

                        self.write_attributes::<BO>(data, num_points, num_cells)?;
                    }
                }

                DataSet::UnstructuredGrid { pieces, .. } => {
                    let piece = pieces
                        .into_iter()
                        .next()
                        .ok_or(DataSetError::MissingPieceData)?;
                    if let Ok(PieceData::UnstructuredGrid {
                        points,
                        cells,
                        data,
                    }) = piece.load_into_piece_data()
                    {
                        writeln!(self, "DATASET UNSTRUCTURED_GRID").map_err(|_| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Tags))
                        })?;

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.data_type())
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
                        let num_verts = cells.cell_verts.num_verts();

                        writeln!(self, "\nCELLS {} {}", num_cells, num_cells + num_verts).map_err(
                            |_| {
                                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                                    EntryPart::Header,
                                )))
                            },
                        )?;

                        let (_, vertices) = cells.cell_verts.into_legacy();

                        self.write_u32_vec::<BO>(vertices).map_err(|e| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::Cells(
                                EntryPart::Data(e.into()),
                            )))
                        })?;

                        writeln!(self, "\nCELL_TYPES {}", cells.types.len()).map_err(|_| {
                            Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                                EntryPart::Header,
                            )))
                        })?;

                        self.write_cell_types::<BO>(cells.types)?;

                        self.write_attributes::<BO>(data, num_points, num_cells as usize)?;
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
                    if let Ok(PieceData::ImageData { data, .. }) = piece.load_into_piece_data() {
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
                    if let Ok(PieceData::StructuredGrid { points, data, .. }) =
                        piece.load_into_piece_data()
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

                        writeln!(self, "POINTS {} {}", points.len() / 3, points.data_type())
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
                    if let Ok(PieceData::RectilinearGrid { coords, data, .. }) =
                        piece.load_into_piece_data()
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
                            coords.x.data_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::XCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_x_coords = coords.x.len();
                        self.write_buf::<BO>(coords.x.data).map_err(|e| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::XCoordinates(EntryPart::Data(e.into())),
                            ))
                        })?;
                        writeln!(
                            self,
                            "Y_COORDINATES {} {}",
                            coords.y.len(),
                            coords.y.data_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::YCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_y_coords = coords.y.len();
                        self.write_buf::<BO>(coords.y.data).map_err(|e| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::YCoordinates(EntryPart::Data(e.into())),
                            ))
                        })?;
                        writeln!(
                            self,
                            "Z_COORDINATES {} {}",
                            coords.z.len(),
                            coords.z.data_type()
                        )
                        .map_err(|_| {
                            Error::DataSet(DataSetError::RectilinearGrid(
                                DataSetPart::ZCoordinates(EntryPart::Header),
                            ))
                        })?;
                        let num_z_coords = coords.z.len();
                        self.write_buf::<BO>(coords.z.data).map_err(|e| {
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
    impl WriteVtkImpl for Vec<u8> {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::io::Write::write_fmt(self, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            writeln!(self, "BINARY\n").map_err(|_| Error::Header(Header::FileType))
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err_fn = |ek: Option<std::io::ErrorKind>| {
                Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                    EntryPart::Data(ek),
                )))
            };
            let err = |e: std::io::Error| err_fn(Some(e.kind()));
            for t in data {
                self.write_i32::<BO>(t as i32).map_err(err)?;
            }
            writeln!(self).map_err(|_| Error::NewLine)
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result {
            let buf = IOBuffer::from(data);
            self.write_buf::<BO>(buf)
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
                IOBuffer::UnsignedChar(v) => write_buf_impl(v, self, Self::write_u8)?,
                IOBuffer::Char(v) => write_buf_impl(v, self, Self::write_i8)?,
                IOBuffer::UnsignedShort(v) => {
                    write_buf_impl(v, self, Self::write_u16::<BO>)?;
                }
                IOBuffer::Short(v) => {
                    write_buf_impl(v, self, Self::write_i16::<BO>)?;
                }
                IOBuffer::UnsignedInt(v) => {
                    write_buf_impl(v, self, Self::write_u32::<BO>)?;
                }
                IOBuffer::Int(v) => {
                    write_buf_impl(v, self, Self::write_i32::<BO>)?;
                }
                IOBuffer::UnsignedLong(v) => {
                    write_buf_impl(v, self, Self::write_u64::<BO>)?;
                }
                IOBuffer::Long(v) => {
                    write_buf_impl(v, self, Self::write_i64::<BO>)?;
                }
                IOBuffer::Float(v) => {
                    write_buf_impl(v, self, Self::write_f32::<BO>)?;
                }
                IOBuffer::Double(v) => {
                    write_buf_impl(v, self, Self::write_f64::<BO>)?;
                }
                _ => return Err(Error::DataMismatchError),
            }

            writeln!(self)
        }
    }

    impl WriteVtkImpl for String {
        fn write_fmt(&mut self, args: Arguments) -> Result {
            std::fmt::Write::write_fmt(self, args)?;
            Ok(())
        }
        fn write_file_type(&mut self) -> Result {
            writeln!(self, "ASCII\n").map_err(|_| Error::Header(Header::FileType))
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>) -> Result {
            let err = Error::DataSet(DataSetError::UnstructuredGrid(DataSetPart::CellTypes(
                EntryPart::Data(None),
            )));
            for t in data {
                writeln!(self, "{}", t as u8).map_err(|_| err)?;
            }
            writeln!(self).map_err(|_| err)
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>) -> Result {
            for i in 0..data.len() {
                write!(self, "{}", data[i])?;
                if i < data.len() - 1 {
                    // add an extra space between elements
                    write!(self, " ")?;
                }
            }
            writeln!(self) // finish with a new line
        }

        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer) -> Result {
            writeln!(self, "{}", data)
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
    fn write_vtk_le(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<LittleEndian>(vtk)
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in big endian format.
    fn write_vtk_be(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<BigEndian>(vtk)
    }
    /// Same as `write_vtk` but overrides the `byte_order` field to write in native endian format.
    fn write_vtk_ne(&mut self, vtk: Vtk) -> Result<&mut Self, Error> {
        self.write_vtk_impl::<NativeEndian>(vtk)
    }
}

impl WriteVtk for Vec<u8> {}
impl WriteVtk for String {}
