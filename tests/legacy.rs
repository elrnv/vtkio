use nom::IResult;
use vtkio::model::*;
use vtkio::parser::*;
use vtkio::writer::*;
use vtkio::Error;

macro_rules! test {
    ($fn:ident ($in:expr, $($args:expr),*) => ($rem:expr, $out:expr)) => {
        assert_eq!($fn($in.as_bytes(), $($args),*), IResult::Ok(($rem.as_bytes(), $out.clone())));
    };
    ($fn:ident ($in:expr) => ($rem:expr, $out:expr)) => {
        assert_eq!($fn($in.as_bytes()), IResult::Ok(($rem.as_bytes(), $out.clone())));
    };
    ($fn:ident ($in:expr, $($args:expr),*) => $out:expr) => {
        test!($fn($in, $($args),*) => ("", $out));
    };
    ($fn:ident ($in:expr) => $out:expr) => {
        test!($fn($in) => ("", $out));
    }
}

macro_rules! test_b {
    ($fn:ident ($in:expr, $($args:expr),*) => $out:expr) => {
        assert_eq!($fn($in, $($args),*), IResult::Ok(("".as_bytes(), $out.clone())));
    };
    ($fn:ident ($in:expr) => $out:expr) => {
        assert_eq!($fn($in), IResult::Ok(("".as_bytes(), $out.clone())));
    };
}

macro_rules! test_ignore_rem {
    ($fn:ident ($in:expr, $($args:expr),*) => $out:expr) => {
        {
            let result = $fn($in, $($args),*);
            assert!(result.is_ok());
            assert_eq!(result.unwrap().1, $out.clone());
        }
    };
    ($fn:ident ($in:expr) => $out:expr) => {
        {
            let result = $fn($in);
            assert!(result.is_ok());
            assert_eq!(result.unwrap().1, $out.clone());
        }
    };
}

type Result = std::result::Result<(), Error>;

// Helper functions to convert between endianness.

fn ne(vtk: &Vtk) -> Vtk {
    Vtk {
        byte_order: ByteOrder::native(),
        ..vtk.clone()
    }
}

fn le(vtk: &Vtk) -> Vtk {
    Vtk {
        byte_order: ByteOrder::LittleEndian,
        ..vtk.clone()
    }
}

// Test paraview output of a single tet
#[test]
fn para_tet_test() -> Result {
    let in1 = include_bytes!("../assets/para_tet.vtk");
    let in2 = include_str!("../assets/para_tet_ascii.vtk").as_bytes();
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("vtk output"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.0f64, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![4, 0, 1, 2, 3],
                },
                types: vec![CellType::Tetra],
            },
            data: Attributes {
                point: vec![],
                cell: vec![Attribute::field("FieldData")
                    .add_field_data(FieldArray::new("FloatValue", 1).with_data(vec![0.0f32]))],
            },
        }),
    };
    test_ignore_rem!(parse_be(in1) => out1);
    test_ignore_rem!(parse_be(in2) => out1);
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

// Test paraview output of a few tets
#[test]
fn para_tets_test() -> Result {
    let in1 = include_bytes!("../assets/para_test.vtk");
    let in2 = include_str!("../assets/para_test_ascii.vtk").as_bytes();
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("vtk output"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                13.2, 135.4, -7.7, 13.7, 134.2, -8.7, 12.2, 134.7, -8.6, 12.7, 133.6, -7.0, 3.6,
                119.4, -0.3, -2.3, 137.0, -2.5, 5.4, 119.7, 0.0, -2.7, 135.9, -1.2, -2.9, 137.5,
                -1.2, -1.8, 136.6, -1.7, 4.3, 119.7, 0.4, 4.6, 118.7, -0.002,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 3,
                    vertices: vec![4, 9, 5, 7, 8, 4, 3, 2, 0, 1, 4, 11, 6, 4, 10],
                },
                types: vec![CellType::Tetra; 3],
            },
            data: Attributes {
                point: vec![Attribute::Field {
                    name: String::from("FieldData"),
                    data_array: vec![
                        FieldArray {
                            name: String::from("Zeros"),
                            elem: 1,
                            data: vec![0.0f32; 12].into(),
                        },
                        FieldArray {
                            name: String::from("Floats"),
                            elem: 1,
                            data: vec![
                                2.3, 2.5, 2.3, 2.1, 1.4, 0.8, 1.6, 0.7, 0.8, 0.7, 1.5, 1.6f32,
                            ]
                            .into(),
                        },
                        FieldArray {
                            name: String::from("Ints"),
                            elem: 1,
                            data: vec![1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0i32].into(),
                        },
                        FieldArray {
                            name: String::from("NegativeInts"),
                            elem: 1,
                            data: vec![-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1i32].into(),
                        },
                        FieldArray {
                            name: String::from("MixedInts"),
                            elem: 1,
                            data: vec![2, -1, 3, -1, -1, -1, -1, -1, -1, 0, -1, 1i32].into(),
                        },
                    ],
                }],
                cell: vec![Attribute::field("FieldData")
                    .add_field_data(FieldArray::new("Ones", 1).with_data(vec![1.0f32; 3]))
                    .add_field_data(FieldArray::new("Zeros", 1).with_data(vec![0.0f32; 3]))],
            },
        }),
    };
    test_ignore_rem!(parse_be(in1) => out1);
    test_ignore_rem!(parse_be(in2) => out1);
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn tet_test() -> Result {
    let in1 = include_str!("../assets/tet.vtk");
    let in2 = include_bytes!("../assets/tet_test.vtk");
    let in3 = include_bytes!("../assets/tet_test_binary.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Tetrahedron example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.0f32, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![4, 0, 1, 2, 3],
                },
                types: vec![CellType::Tetra],
            },
            data: Attributes::new(),
        }),
    };
    test!(parse_le(in1) => le(&out1));
    test_b!(parse_le(in2) => le(&out1));
    test_b!(parse_be(in3) => out1);
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn tri_test() -> Result {
    let in1 = include_str!("../assets/tri.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Triangle example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            polys: Some(VertexNumbers::Legacy {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            }),
            data: Attributes::new(),
            ..Default::default()
        }),
    };
    test!(parse_be(in1) => out1);
    test!(parse_le(in1) => le(&out1));
    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    Ok(())
}

#[test]
fn tri_attrib_ascii_test() -> Result {
    let in1 = include_str!("../assets/tri_attrib.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Triangle example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            polys: Some(VertexNumbers::Legacy {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            }),
            data: Attributes {
                point: vec![],
                cell: vec![
                    Attribute::DataArray(DataArray {
                        name: String::from("scalars"),
                        elem: ElementType::ColorScalars(3),
                        data: vec![1.0f32, 0.0, 0.0].into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("tex_coords"),
                        elem: ElementType::TCoords(3),
                        data: vec![1.0f32, 0.0, 0.0].into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("tensors"),
                        elem: ElementType::Tensors,
                        data: vec![1.0f64, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0].into(),
                    }),
                ],
            },
            ..Default::default()
        }),
    };
    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    // Color scalars are floats only in the ASCII vtk file format. Thus we have another test for
    // The same file but in binary in tri_attrib_binary_test.
    Ok(())
}

#[test]
fn tri_attrib_binary_test() -> Result {
    let in1 = include_bytes!("../assets/tri_attrib_binary.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Triangle example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            polys: Some(VertexNumbers::Legacy {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            }),
            data: Attributes {
                point: vec![],
                cell: vec![
                    Attribute::DataArray(DataArray {
                        name: String::from("scalars"),
                        elem: ElementType::ColorScalars(3),
                        data: vec![255u8, 0, 0].into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("tex_coords"),
                        elem: ElementType::TCoords(3),
                        data: vec![1.0f32, 0.0, 0.0].into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("tensors"),
                        elem: ElementType::Tensors,
                        data: vec![1.0f64, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0].into(),
                    }),
                ],
            },
            ..Default::default()
        }),
    };
    test_ignore_rem!(parse_be(in1) => out1);
    //test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => out1);
    // Color scalars are u8 only in the Binary vtk file format. ASCII style color scalars are
    // stored as floats and are tested in tri_attrib_ascii_test.
    Ok(())
}

#[test]
fn square_test() -> Result {
    let in1 = include_str!("../assets/square.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Square example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: vec![
                0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0, -1.0,
            ]
            .into(),
            polys: Some(VertexNumbers::Legacy {
                num_cells: 1,
                vertices: vec![4, 0, 1, 2, 3],
            }),
            data: Attributes::new(),
            ..Default::default()
        }),
    };
    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn cube_test() -> Result {
    let in1 = include_str!("../assets/cube.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Cube example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0,
                0.0, -1.0, 1.0, 1.0, 0.0, 1.0, 1.0, -1.0f32,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![8, 0, 4, 5, 1, 2, 6, 7, 3],
                },
                types: vec![CellType::Hexahedron],
            },
            data: Attributes::new(),
        }),
    };

    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn structured_grid_test() -> Result {
    let in1 = include_str!("../assets/structured_grid.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(3, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("vtk output"),
        file_path: None,
        data: DataSet::inline(StructuredGridPiece {
            extent: Extent::Dims([2, 2, 2]),
            points: vec![
                0_f32, 0.2, 0., 0.1, 0.184843, 0., 0., 0.25, 0., 0.1, 0.234843, 0., 0., 0.2,
                0.333333, 0.1, 0.184843, 0.333333, 0., 0.25, 0.333333, 0.1, 0.234843, 0.333333,
            ]
            .into(),
            data: Attributes {
                point: vec![
                    Attribute::DataArray(DataArray {
                        name: String::from("ptval"),
                        elem: ElementType::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                        },
                        data: vec![0f32, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0].into(),
                    }),
                    Attribute::vectors("ptvec").with_data(vec![
                        0_f32, 0.0287671, 0., 0., 0.0258604, 0., 0., 0.0287671, 0., 0., 0.0258604,
                        0., 0., 0.0287671, 0., 0., 0.0258604, 0., 0., 0.0287671, 0., 0., 0.0258604,
                        0.,
                    ]),
                ],
                cell: vec![
                    Attribute::DataArray(DataArray {
                        name: String::from("cellval"),
                        elem: ElementType::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                        },
                        data: vec![1489.0f32].into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("cellvec"),
                        elem: ElementType::Vectors,
                        data: vec![0.6f32, 0.7, 0.5].into(),
                    }),
                ],
            },
        }),
    };

    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn rectilinear_grid_test() -> Result {
    let in1 = include_bytes!("../assets/rectilinear_grid.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(3, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("vtk output"),
        file_path: None,
        data: DataSet::inline(RectilinearGridPiece {
            extent: Extent::Dims([3, 4, 1]),
            coords: Coordinates {
                x: vec![0_f32, 2.0, 4.0].into(),
                y: vec![1_f32, 2.0, 3.0, 4.0].into(),
                z: vec![0_f32].into(),
            },
            data: Attributes {
                point: vec![],
                cell: vec![Attribute::Field {
                    name: String::from("FieldData"),
                    data_array: vec![FieldArray {
                        name: String::from("cellscalar"),
                        elem: 1,
                        data: vec![1.1_f32, 7.5, 1.2, 1.5, 2.6, 8.1].into(),
                    }],
                }],
            },
        }),
    };
    test_b!(parse_be(in1) => out1);
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);

    // Same thing should work in binary
    let in1 = include_bytes!("../assets/rectilinear_grid_binary.vtk");
    let out2 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        ..out1
    };
    test_b!(parse_be(in1) => out2);
    Ok(())
}

#[test]
fn field_test() -> Result {
    let in1 = include_bytes!("../assets/field.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("field example"),
        file_path: None,
        data: DataSet::Field {
            name: String::from("FieldData"),
            data_array: vec![
                FieldArray {
                    name: String::from("cellIds"),
                    elem: 1,
                    data: vec![0, 1, 2, 3, 4, 5].into(),
                },
                FieldArray {
                    name: String::from("faceAttributes"),
                    elem: 2,
                    data: vec![0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6.].into(),
                },
            ],
        },
    };
    test_b!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn cube_complex_test() -> Result {
    let polys = Some(VertexNumbers::Legacy {
        num_cells: 6,
        vertices: vec![
            4, 0, 1, 2, 3, 4, 4, 5, 6, 7, 4, 0, 1, 5, 4, 4, 2, 3, 7, 6, 4, 0, 4, 7, 3, 4, 1, 2, 6,
            5,
        ],
    });

    let mut attributes = Attributes {
        point: vec![
            Attribute::DataArray(DataArray {
                name: String::from("sample_scalars"),
                elem: ElementType::Scalars {
                    num_comp: 1,
                    lookup_table: Some(String::from("my_table")),
                },
                data: vec![0.0f32, 1., 2., 3., 4., 5., 6., 7.].into(),
            }),
            Attribute::DataArray(DataArray {
                name: String::from("my_table"),
                elem: ElementType::LookupTable,
                data: vec![
                    0.0f32, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0,
                    1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                    1.0,
                ]
                .into(),
            }),
        ],
        cell: vec![
            Attribute::DataArray(DataArray {
                name: String::from("cell_scalars"),
                elem: ElementType::Scalars {
                    num_comp: 1,
                    lookup_table: None,
                },
                data: vec![0, 1, 2, 3, 4, 5].into(),
            }),
            Attribute::DataArray(DataArray {
                name: String::from("cell_normals"),
                elem: ElementType::Normals,
                data: vec![
                    0.0f32, 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., 0.,
                ]
                .into(),
            }),
            Attribute::Field {
                name: String::from("FieldData"),
                data_array: vec![
                    FieldArray {
                        name: String::from("cellIds"),
                        elem: 1,
                        data: vec![0, 1, 2, 3, 4, 5].into(),
                    },
                    FieldArray {
                        name: String::from("faceAttributes"),
                        elem: 2,
                        data: vec![0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6.].into(),
                    },
                ],
            },
        ],
    };
    let points: IOBuffer = vec![
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0f32, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0,
        1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0,
    ]
    .into();

    let in1 = include_str!("../assets/cube_complex.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Cube example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: points.clone(),
            polys: polys.clone(),
            data: attributes.clone(),
            ..Default::default()
        }),
    };

    let in2 = include_bytes!("../assets/cube_complex_topo.vtk");

    let verts = Some(VertexNumbers::Legacy {
        num_cells: 2,
        vertices: vec![2, 0, 1, 2, 2, 3],
    });

    attributes.cell = vec![
        Attribute::DataArray(DataArray {
            name: String::from("cell_scalars"),
            elem: ElementType::Scalars {
                num_comp: 1,
                lookup_table: None,
            },
            data: vec![0, 1, 2, 3, 4, 5, 6, 7].into(),
        }),
        Attribute::DataArray(DataArray {
            name: String::from("cell_normals"),
            elem: ElementType::Normals,
            data: vec![
                0.0f32, 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., 0., 1.,
                0., 0., 1., 0., 0.,
            ]
            .into(),
        }),
        Attribute::Field {
            name: String::from("FieldData"),
            data_array: vec![
                FieldArray {
                    name: String::from("cellIds"),
                    elem: 1,
                    data: vec![0, 1, 2, 3, 4, 5, 7, 8].into(),
                },
                FieldArray {
                    name: String::from("faceAttributes"),
                    elem: 2,
                    data: vec![
                        0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6., 6., 6., 7., 7.,
                    ]
                    .into(),
                },
            ],
        },
    ];

    let out2 = Vtk {
        data: DataSet::inline(PolyDataPiece {
            points: points.clone(),
            polys: polys.clone(),
            verts: verts.clone(),
            data: attributes.clone(),
            ..Default::default()
        }),
        ..out1.clone()
    };

    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    test_b!(parse_ne(in2) => ne(&out2));
    test_b!(parse_ne(String::new().write_vtk_ne(out2.clone())?.as_bytes()) => ne(&out2));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out2.clone())?) => le(&out2));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out2.clone())?) => out2);
    Ok(())
}

#[test]
fn unstructured_grid_complex_test() -> Result {
    let in1 = include_str!("../assets/unstructured_grid_complex.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Unstructured Grid Example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.0f32, 0., 0., 1., 0., 0., 2., 0., 0., 0., 1., 0., 1., 1., 0., 2., 1., 0., 0., 0.,
                1., 1., 0., 1., 2., 0., 1., 0., 1., 1., 1., 1., 1., 2., 1., 1., 0., 1., 2., 1., 1.,
                2., 2., 1., 2., 0., 1., 3., 1., 1., 3., 2., 1., 3., 0., 1., 4., 1., 1., 4., 2., 1.,
                4., 0., 1., 5., 1., 1., 5., 2., 1., 5., 0., 1., 6., 1., 1., 6., 2., 1., 6.,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 12,
                    vertices: vec![
                        10, 0, 1, 4, 3, 6, 7, 10, 9, 2, 5, 8, 0, 1, 4, 3, 6, 7, 10, 9, 8, 1, 2, 5,
                        4, 7, 8, 11, 10, 4, 6, 10, 9, 12, 4, 5, 11, 10, 14, 6, 15, 16, 17, 14, 13,
                        12, 6, 18, 15, 19, 16, 20, 17, 4, 22, 23, 20, 19, 3, 21, 22, 18, 3, 22, 19,
                        18, 2, 26, 25, 1, 24,
                    ],
                },
                types: vec![
                    CellType::QuadraticTetra,
                    CellType::Hexahedron,
                    CellType::Hexahedron,
                    CellType::Tetra,
                    CellType::Tetra,
                    CellType::Polygon,
                    CellType::TriangleStrip,
                    CellType::Quad,
                    CellType::Triangle,
                    CellType::Triangle,
                    CellType::Line,
                    CellType::Vertex,
                ],
            },
            data: Attributes {
                point: vec![
                    Attribute::DataArray(DataArray {
                        name: String::from("scalars"),
                        elem: ElementType::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                        },
                        data: vec![
                            0.0f32, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0,
                            13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0,
                            25.0, 26.0,
                        ]
                        .into(),
                    }),
                    Attribute::DataArray(DataArray {
                        name: String::from("vectors"),
                        elem: ElementType::Vectors,
                        data: vec![
                            1.0f32, 0., 0., 1., 1., 0., 0., 2., 0., 1., 0., 0., 1., 1., 0., 0., 2.,
                            0., 1., 0., 0., 1., 1., 0., 0., 2., 0., 1., 0., 0., 1., 1., 0., 0., 2.,
                            0., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0.,
                            1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0.,
                            1., 0., 0., 1., 0., 0., 1., 0., 0., 1.,
                        ]
                        .into(),
                    }),
                ],
                cell: vec![],
            },
        }),
    };
    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));

    test_b!(parse_le(Vec::<u8>::new().write_vtk(le(&out1))?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn volume_complex_test() -> Result {
    let in1 = include_str!("../assets/volume_complex.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Volume example"),
        file_path: None,
        data: DataSet::inline(ImageDataPiece {
            extent: Extent::Dims([3, 4, 6]),
            data: Attributes {
                point: vec![Attribute::DataArray(DataArray {
                    name: String::from("volume_scalars"),
                    elem: ElementType::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                    },
                    data: vec![
                        0i8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 15, 20, 25, 25, 20, 15, 10,
                        5, 0, 0, 10, 20, 30, 40, 50, 50, 40, 30, 20, 10, 0, 0, 10, 20, 30, 40, 50,
                        50, 40, 30, 20, 10, 0, 0, 5, 10, 15, 20, 25, 25, 20, 15, 10, 5, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                    ]
                    .into(),
                })],
                cell: vec![],
            },
        }),
    };
    test!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk(le(&out1))?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn dodecagon_test() -> Result {
    let in1 = include_bytes!("../assets/dodecagon_ascii_simple.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Dodecagon example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.5f32,
                0.,
                0.,
                0.433013,
                0.,
                -0.25,
                0.25,
                0.,
                -0.433013,
                3.06162e-17,
                0.,
                -0.5,
                -0.25,
                0.,
                -0.433013,
                -0.433013,
                0.,
                -0.25,
                -0.5,
                0.,
                -6.12323e-17,
                -0.433013,
                0.,
                0.25,
                -0.25,
                0.,
                0.433013,
                -9.18485e-17,
                0.,
                0.5,
                0.25,
                0.,
                0.433013,
                0.433013,
                0.,
                0.25,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
                },
                types: vec![CellType::Polygon],
            },
            data: Attributes::new(),
        }),
    };

    test_b!(parse_ne(in1) => ne(&out1));
    test_b!(parse_ne(String::new().write_vtk_ne(out1.clone())?.as_bytes()) => ne(&out1));
    test_b!(parse_ne(Vec::<u8>::new().write_vtk_ne(out1.clone())?) => ne(&out1));
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => le(&out1));
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn dodecagon_with_meta_test() {
    let in1 = include_bytes!("../assets/dodecagon_ascii.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Dodecagon example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.5f32,
                0.,
                0.,
                0.433013,
                0.,
                -0.25,
                0.25,
                0.,
                -0.433013,
                3.06162e-17,
                0.,
                -0.5,
                -0.25,
                0.,
                -0.433013,
                -0.433013,
                0.,
                -0.25,
                -0.5,
                0.,
                -6.12323e-17,
                -0.433013,
                0.,
                0.25,
                -0.25,
                0.,
                0.433013,
                -9.18485e-17,
                0.,
                0.5,
                0.25,
                0.,
                0.433013,
                0.433013,
                0.,
                0.25,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
                },
                types: vec![CellType::Polygon],
            },
            data: Attributes::new(),
        }),
    };

    test_b!(parse_ne(in1) => ne(&out1));
}

#[test]
fn dodecagon_with_meta_line_endings_test() {
    let in1 = include_str!("../assets/dodecagon_ascii.vtk");
    // Make sure that all line-endings are LF endings (in case the file was checked out with git
    // configured to use CRLF endings)
    let in1_lf = in1.replace("\r\n", "\n");
    // Create input using CRLF endings instead of LF endings
    let in1_crlf = in1_lf.replace("\n", "\r\n");

    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Dodecagon example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.5f32,
                0.,
                0.,
                0.433013,
                0.,
                -0.25,
                0.25,
                0.,
                -0.433013,
                3.06162e-17,
                0.,
                -0.5,
                -0.25,
                0.,
                -0.433013,
                -0.433013,
                0.,
                -0.25,
                -0.5,
                0.,
                -6.12323e-17,
                -0.433013,
                0.,
                0.25,
                -0.25,
                0.,
                0.433013,
                -9.18485e-17,
                0.,
                0.5,
                0.25,
                0.,
                0.433013,
                0.433013,
                0.,
                0.25,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
                },
                types: vec![CellType::Polygon],
            },
            data: Attributes::new(),
        }),
    };

    test_b!(parse_ne(in1_lf.as_bytes()) => ne(&out1));
    test_b!(parse_ne(in1_crlf.as_bytes()) => ne(&out1));
}

#[test]
fn binary_dodecagon_test() {
    let in1 = include_bytes!("../assets/dodecagon_simple.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(4, 2),
        byte_order: ByteOrder::BigEndian,
        title: String::from("Dodecagon example"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                0.5f32,
                0.,
                0.,
                0.4330127,
                0.,
                -0.25,
                0.25,
                0.,
                -0.4330127,
                3.061617e-17,
                0.,
                -0.5,
                -0.25,
                0.,
                -0.4330127,
                -0.4330127,
                0.,
                -0.25,
                -0.5,
                0.,
                -6.123234e-17,
                -0.4330127,
                0.,
                0.25,
                -0.25,
                0.,
                0.4330127,
                -9.184851e-17,
                0.,
                0.5,
                0.25,
                0.,
                0.4330127,
                0.4330127,
                0.,
                0.25,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
                },
                types: vec![CellType::Polygon],
            },
            data: Attributes::new(),
        }),
    };
    let in2 = include_bytes!("../assets/dodecagon.vtk");

    test_b!(parse_be(in1) => out1);
    test_b!(parse_be(in2) => out1);
}

#[test]
fn triangle_vtkidtype_test() {
    let in1 = include_bytes!("../assets/triangle_vtkidtype.vtk");
    let out1 = Vtk {
        version: Version::new_legacy(5, 1),
        byte_order: ByteOrder::BigEndian,
        title: String::from("vtk output"),
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: vec![
                1.837499976158142,
                2.9874446392059326,
                0.23750001192092896,
                1.8249998092651367,
                2.9850914478302,
                0.23750001192092896,
                1.8249998092651367,
                2.985128402709961,
                0.25,
            ]
            .into(),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![1, 2, 0],
                    offsets: vec![0, 3],
                },
                types: vec![CellType::Triangle],
            },
            data: Attributes {
                point: vec![Attribute::Field {
                    name: String::from("FieldData"),
                    data_array: vec![DataArrayBase {
                        name: String::from("vtkOriginalPointIds"),
                        elem: 1,
                        data: vec![95364, 95538, 95691].into(),
                    }],
                }],
                cell: vec![Attribute::Field {
                    name: String::from("FieldData"),
                    data_array: vec![DataArrayBase {
                        name: String::from("vtkOriginalCellIds"),
                        elem: 1,
                        data: vec![186229].into(),
                    }],
                }],
            },
        }),
    };

    test_b!(parse_be(in1) => out1);
}

#[test]
fn point_cloud() -> Result {
    let in1 = include_bytes!("../assets/point_cloud.vtk");
    let vtk = Vtk {
        version: Version::new_legacy(2, 0),
        byte_order: ByteOrder::BigEndian,
        title: String::from("PointCloud example"),
        file_path: None,
        data: DataSet::inline(PolyDataPiece {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            verts: Some(VertexNumbers::Legacy {
                num_cells: 3,
                vertices: vec![1, 0, 1, 1, 1, 2],
            }),
            data: Attributes::new(),
            ..Default::default()
        }),
    };
    test_b!(parse_be(in1) => vtk);
    test_b!(parse_be(Vec::<u8>::new().write_vtk(vtk.clone())?) => vtk);
    Ok(())
}
