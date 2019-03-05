use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};
use model::*;
use std::fmt::Arguments;
use IOBuffer;

mod write_vtk_impl {
    use super::*;
    use byteorder::WriteBytesExt;
    use num_traits::ToPrimitive;

    #[derive(Copy, Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub struct Error;

    pub trait WriteVtkImpl {
        /// This function is called by the `write!` macro used throughout this module.
        /// Each writer needs to call the appropriate `write_fmt` in the implementation
        /// of this method.
        fn write_fmt(&mut self, args: Arguments) -> Result<(), Error>;
        fn write_file_type(&mut self, err: &str);
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>, err: &str);
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>, err: &str);
        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer, err: &str);

        fn write_attrib<BO: ByteOrder>(
            &mut self,
            data: Attributes,
            num_points: usize,
            num_cells: usize,
        ) {
            write!(self, "\nCELL_DATA {}\n", num_cells).expect("Error writing cell data header.");
            self.write_attrib_data::<BO>(data.cell);

            write!(self, "\nPOINT_DATA {}\n", num_points)
                .expect("Error writing point data header.");
            self.write_attrib_data::<BO>(data.point);
        }

        fn write_attrib_data<BO: ByteOrder>(&mut self, data: Vec<(String, Attribute)>) {
            for (name, attrib) in data {
                write!(self, "\n");
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
                        ).expect("Error writing a Scalars attribute header.");
                        write!(
                            self,
                            "LOOKUP_TABLE {}\n",
                            lookup_table.unwrap_or(String::from("default"))
                        ).expect("Error writing a lookup table name for Scalars data.");
                        self.write_buf::<BO>(data, "Error writing Scalars attribute data.");
                    }
                    Attribute::ColorScalars { num_comp, data } => {
                        write!(self, "COLOR_SCALARS {} {}\n", name, num_comp)
                            .expect("Error writing a ColorScalars attribute header.");
                        self.write_buf::<BO>(data, "Error writing ColorScalars attribute data.");
                    }
                    Attribute::LookupTable { data } => {
                        write!(self, "LOOKUP_TABLE {} {}\n", name, data.len() / 4)
                            .expect("Error writing a LookupTable attribute header.");
                        self.write_buf::<BO>(data, "Error writing LookupTable attribute data.");
                    }
                    Attribute::Vectors { data } => {
                        write!(
                            self,
                            "VECTORS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        ).expect("Error writing a Vectors attribute header.");
                        self.write_buf::<BO>(data, "Error writing Vectors attribute data.");
                    }
                    Attribute::Normals { data } => {
                        write!(
                            self,
                            "NORMALS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        ).expect("Error writing a Normals attribute header.");
                        self.write_buf::<BO>(data, "Error writing Normals attribute data.");
                    }
                    Attribute::TextureCoordinates { dim, data } => {
                        write!(
                            self,
                            "TEXTURE_COORDINATES {} {} {}\n",
                            name,
                            dim,
                            DataType::from(data.element_type_id())
                        ).expect("Error writing a TextureCoordinates attribute header.");
                        self.write_buf::<BO>(
                            data,
                            "Error writing TextureCoordinates attribute data.",
                        );
                    }
                    Attribute::Tensors { data } => {
                        write!(
                            self,
                            "TENSORS {} {}\n",
                            name,
                            DataType::from(data.element_type_id())
                        ).expect("Error writing a Tensors attribute header.");
                        self.write_buf::<BO>(data, "Error writing Tensors attribute data.");
                    }
                    Attribute::Field { data_array } => {
                        write!(self, "FIELD {} {}\n", name, data_array.len())
                            .expect("Error writing a Field attribute header.");
                        for field in data_array {
                            write!(
                                self,
                                "{} {} {} {}\n",
                                field.name,
                                field.num_comp,
                                field.data.len() / field.num_comp as usize,
                                DataType::from(field.data.element_type_id())
                            ).expect("Error writing Field attribute array header.");
                            self.write_buf::<BO>(
                                field.data,
                                "Error writing Field attribute array data.",
                            );
                        }
                    }
                }
            }
        }
        fn write_vtk_impl<BO: ByteOrder>(&mut self, vtk: Vtk) -> &mut Self {
            write!(self, "# vtk DataFile Version {}\n", vtk.version)
                .expect("Error writing VTK Header.");
            write!(self, "{}\n", vtk.title).expect("Error writing VTK Title.");
            self.write_file_type("Error writing File Type.");
            match vtk.data {
                DataSet::Field { name, data_array } => {
                    write!(self, "FIELD {} {}\n", name, data_array.len())
                        .expect("Error writing Field data header.");
                    for field in data_array {
                        write!(
                            self,
                            "{} {} {} {}\n",
                            field.name,
                            field.num_comp,
                            field.data.len() / field.num_comp as usize,
                            DataType::from(field.data.element_type_id())
                        ).expect("Error writing Field array data header.");
                        self.write_buf::<BO>(
                            field.data,
                            "Error writing a new line after binary data.",
                        );
                    }
                }

                DataSet::PolyData { points, topo, data } => {
                    write!(self, "DATASET POLYDATA\n").expect("Error writing PolyData tags.");

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    ).expect("Error writing PolyData points.");
                    let num_points = points.len() / 3;
                    self.write_buf::<BO>(points, "Error writing PolyData points.");

                    write!(self, "\n");

                    let mut num_cells = 0;
                    for topo_type in topo {
                        match topo_type {
                            PolyDataTopology::Vertices(_) => write!(self, "VERTICES"),
                            PolyDataTopology::Lines(_) => write!(self, "LINES"),
                            PolyDataTopology::Polygons(_) => write!(self, "POLYGONS"),
                            PolyDataTopology::TriangleStrips(_) => write!(self, "TRIANGLE_STRIPS"),
                        }.expect("Error writing PolyData topology tag.");

                        let cells = match topo_type {
                            PolyDataTopology::Vertices(c) => c,
                            PolyDataTopology::Lines(c) => c,
                            PolyDataTopology::Polygons(c) => c,
                            PolyDataTopology::TriangleStrips(c) => c,
                        };

                        write!(self, " {} {}\n", cells.num_cells, cells.vertices.len())
                            .expect("Error writing PolyData topology sizes.");

                        self.write_u32_vec::<BO>(
                            cells.vertices,
                            "Error writing PolyData topology data.",
                        );

                        num_cells += cells.num_cells as usize;
                    }

                    self.write_attrib::<BO>(data, num_points, num_cells);
                }

                DataSet::UnstructuredGrid {
                    points,
                    cells,
                    cell_types,
                    data,
                } => {
                    write!(self, "DATASET UNSTRUCTURED_GRID\n")
                        .expect("Error writing UnstructuredGrid tags.");

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    ).expect("Error writing UnstructuredGrid points.");
                    let num_points = points.len() / 3;
                    self.write_buf::<BO>(points, "Error writing UnstructuredGrid points.");

                    write!(
                        self,
                        "\nCELLS {} {}\n",
                        cells.num_cells,
                        cells.vertices.len()
                    ).expect("Error writing UnstructuredGrid cell sizes.");

                    self.write_u32_vec::<BO>(
                        cells.vertices,
                        "Error writing UnstructuredGrid cell data.",
                    );

                    write!(self, "\nCELL_TYPES {}\n", cell_types.len())
                        .expect("Error writing UnstructuredGrid cell types size.");

                    let err_cell_types = "Error writing UnstructuredGrid cell types.";
                    self.write_cell_types::<BO>(cell_types, err_cell_types);

                    self.write_attrib::<BO>(data, num_points, cells.num_cells as usize);
                }

                DataSet::StructuredPoints {
                    dims,
                    origin,
                    spacing,
                    data,
                } => {
                    write!(self, "DATASET STRUCTURED_POINTS\n")
                        .expect("Error writing StructuredPoints tags.");

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2])
                        .expect("Error writing StructuredPoints dimensions.");

                    write!(self, "ORIGIN {} {} {}\n", origin[0], origin[1], origin[2])
                        .expect("Error writing StructuredPoints origin.");

                    if vtk.version.major < 2 {
                        write!(self, "ASPECT_RATIO")
                    } else {
                        write!(self, "SPACING")
                    }.expect("Error writing StructuredPoints spacing.");
                    write!(self, " {} {} {}\n", spacing[0], spacing[1], spacing[2])
                        .expect("Error writing StructuredPoints spacing.");

                    let num_points = (dims[0] * dims[1] * dims[2]) as usize;
                    self.write_attrib::<BO>(data, num_points, 0);
                }

                DataSet::StructuredGrid { dims, points, data } => {
                    write!(self, "DATASET STRUCTURED_GRID\n")
                        .expect("Error writing StructuredGrid tags.");

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2])
                        .expect("Error writing StructuredGrid dimensions.");

                    write!(
                        self,
                        "POINTS {} {}\n",
                        points.len() / 3,
                        DataType::from(points.element_type_id())
                    ).expect("Error writing StructuredGrid points.");
                    let num_points = points.len() / 3;
                    self.write_buf::<BO>(points, "Error writing StructuredGrid points.");

                    assert_eq!((dims[0] * dims[1] * dims[2]) as usize, num_points);
                    self.write_attrib::<BO>(data, num_points, 1);
                }

                DataSet::RectilinearGrid {
                    dims,
                    x_coords,
                    y_coords,
                    z_coords,
                    data,
                } => {
                    write!(self, "DATASET RECTILINEAR_GRID\n")
                        .expect("Error writing RectilinearGrid tags.");

                    write!(self, "DIMENSIONS {} {} {}\n", dims[0], dims[1], dims[2])
                        .expect("Error writing RectilinearGrid dimensions.");

                    write!(
                        self,
                        "X_COORDINATES {} {}\n",
                        x_coords.len(),
                        DataType::from(x_coords.element_type_id())
                    ).expect("Error writing RectilinearGrid x_coordinates.");
                    let num_x_coords = x_coords.len();
                    self.write_buf::<BO>(x_coords, "Error writing RectilinearGrid x_coordinates.");
                    write!(
                        self,
                        "Y_COORDINATES {} {}\n",
                        y_coords.len(),
                        DataType::from(y_coords.element_type_id())
                    ).expect("Error writing RectilinearGrid y_coordinates.");
                    let num_y_coords = y_coords.len();
                    self.write_buf::<BO>(y_coords, "Error writing RectilinearGrid y_coordinates.");
                    write!(
                        self,
                        "Z_COORDINATES {} {}\n",
                        z_coords.len(),
                        DataType::from(z_coords.element_type_id())
                    ).expect("Error writing RectilinearGrid z_coordinates.");
                    let num_z_coords = z_coords.len();
                    self.write_buf::<BO>(z_coords, "Error writing RectilinearGrid z_coordinates.");

                    let num_points = num_x_coords * num_y_coords * num_z_coords;
                    let num_cells = (num_x_coords - 1) * (num_y_coords - 1) * (num_z_coords - 1);
                    self.write_attrib::<BO>(data, num_points, num_cells);
                }
            }

            write!(self, "\n");
            self
        }
    }
    impl WriteVtkImpl for Vec<u8> {
        fn write_fmt(&mut self, args: Arguments) -> Result<(), Error> {
            match ::std::io::Write::write_fmt(self, args) {
                Ok(v) => Ok(v),
                Err(_) => Err(Error),
            }
        }
        fn write_file_type(&mut self, err: &str) {
            write!(self, "BINARY\n\n").expect(err);
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>, err: &str) {
            for t in data {
                self.write_i32::<BO>(t.to_i32().expect(err)).expect(err);
            }
            write!(self, "\n").expect(err);
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>, err: &str) {
            let buf = IOBuffer::from(data);
            self.write_buf::<BO>(buf, err);
        }
        fn write_buf<BO: ByteOrder>(&mut self, buf: IOBuffer, err: &str) {
            use std::any::TypeId;

            fn write_buf_impl<T, W, E>(buf: IOBuffer, writer: &mut W, elem_writer: E, err: &str)
            where
                W: WriteBytesExt,
                E: Fn(&mut W, T) -> ::std::io::Result<()>,
            {
                for elem in buf.reinterpret_into_vec::<T>() {
                    elem_writer(writer, elem).expect(err);
                }
            }

            match buf.element_type_id() {
                x if x == TypeId::of::<u8>() => write_buf_impl(buf, self, Self::write_u8, err),
                x if x == TypeId::of::<i8>() => write_buf_impl(buf, self, Self::write_i8, err),
                x if x == TypeId::of::<u16>() => {
                    write_buf_impl(buf, self, Self::write_u16::<BO>, err)
                }
                x if x == TypeId::of::<i16>() => {
                    write_buf_impl(buf, self, Self::write_i16::<BO>, err)
                }
                x if x == TypeId::of::<u32>() => {
                    write_buf_impl(buf, self, Self::write_u32::<BO>, err)
                }
                x if x == TypeId::of::<i32>() => {
                    write_buf_impl(buf, self, Self::write_i32::<BO>, err)
                }
                x if x == TypeId::of::<u64>() => {
                    write_buf_impl(buf, self, Self::write_u64::<BO>, err)
                }
                x if x == TypeId::of::<i64>() => {
                    write_buf_impl(buf, self, Self::write_i64::<BO>, err)
                }
                x if x == TypeId::of::<f32>() => {
                    write_buf_impl(buf, self, Self::write_f32::<BO>, err)
                }
                x if x == TypeId::of::<f64>() => {
                    write_buf_impl(buf, self, Self::write_f64::<BO>, err)
                }
                _ => {}
            }

            write!(self, "\n").expect("Error writing a new line after binary data.");
        }
    }

    impl WriteVtkImpl for String {
        fn write_fmt(&mut self, args: Arguments) -> Result<(), Error> {
            match ::std::fmt::Write::write_fmt(self, args) {
                Ok(v) => Ok(v),
                Err(_) => Err(Error),
            }
        }
        fn write_file_type(&mut self, err: &str) {
            write!(self, "ASCII\n\n").expect(err);
        }
        fn write_cell_types<BO: ByteOrder>(&mut self, data: Vec<CellType>, err: &str) {
            for t in data {
                write!(self, "{}\n", t.to_u8().unwrap()).expect(err);
            }
            write!(self, "\n").expect(err);
        }
        fn write_u32_vec<BO: ByteOrder>(&mut self, data: Vec<u32>, err: &str) {
            for i in 0..data.len() {
                write!(self, "{}", data[i]).expect(err);
                if i < data.len() - 1 {
                    // add an extra space between elements
                    write!(self, " ").expect(err);
                }
            }
            write!(self, "\n").expect(err); // finish with a new line
        }

        fn write_buf<BO: ByteOrder>(&mut self, data: IOBuffer, err: &str) {
            write!(self, "{}\n", data).expect(err);
        }
    }
}

pub trait WriteVtk: write_vtk_impl::WriteVtkImpl {
    fn write_vtk(&mut self, vtk: Vtk) -> &mut Self {
        self.write_vtk_impl::<NativeEndian>(vtk)
    }
    fn write_vtk_le(&mut self, vtk: Vtk) -> &mut Self {
        self.write_vtk_impl::<LittleEndian>(vtk)
    }
    fn write_vtk_be(&mut self, vtk: Vtk) -> &mut Self {
        self.write_vtk_impl::<BigEndian>(vtk)
    }
}

impl WriteVtk for Vec<u8> {}
impl WriteVtk for String {}
