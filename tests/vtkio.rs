extern crate nom;
extern crate num_traits;
extern crate vtkio;

use nom::IResult;
use vtkio::model::*;
use vtkio::parser::*;
use vtkio::writer::*;
use vtkio::Error;
use vtkio::IOBuffer;

macro_rules! test {
    ($fn:ident ($in:expr, $($args:expr),*) => ($rem:expr, $out:expr)) => {
        assert_eq!($fn($in.as_bytes(), $($args),*), IResult::Done($rem.as_bytes(), $out.clone()));
    };
    ($fn:ident ($in:expr) => ($rem:expr, $out:expr)) => {
        assert_eq!($fn($in.as_bytes()), IResult::Done($rem.as_bytes(), $out.clone()));
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
        assert_eq!($fn($in, $($args),*), IResult::Done("".as_bytes(), $out.clone()));
    };
    ($fn:ident ($in:expr) => $out:expr) => {
        assert_eq!($fn($in), IResult::Done("".as_bytes(), $out.clone()));
    };
}

macro_rules! test_ignore_rem {
    ($fn:ident ($in:expr, $($args:expr),*) => $out:expr) => {
        {
            let result = $fn($in, $($args),*);
            assert!(result.is_done());
            assert_eq!(result.unwrap().1, $out.clone());
        }
    };
    ($fn:ident ($in:expr) => $out:expr) => {
        {
            let result = $fn($in);
            assert!(result.is_done());
            assert_eq!(result.unwrap().1, $out.clone());
        }
    };
}

type Result = std::result::Result<(), Error>;

// Test paraview output of a single tet
#[test]
fn para_tet_test() -> Result {
    let in1 = include_bytes!("../assets/para_tet.vtk");
    let in2 = include_str!("../assets/para_tet_ascii.vtk").as_bytes();
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("vtk output"),
        data: DataSet::UnstructuredGrid {
            points: vec![
                0.0f64, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0,
            ]
            .into(),
            cells: Cells {
                num_cells: 1,
                vertices: vec![4, 0, 1, 2, 3],
            },
            cell_types: vec![CellType::Tetra],
            data: Attributes {
                point: vec![],
                cell: vec![(
                    String::from("FieldData"),
                    Attribute::Field {
                        data_array: vec![FieldArray {
                            name: String::from("FloatValue"),
                            num_comp: 1,
                            data: vec![0.0f32].into(),
                        }],
                    },
                )],
            },
        },
    };
    test_ignore_rem!(parse_be(in1) => out1);
    test_ignore_rem!(parse_be(in2) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

// Test paraview output of a few tets
#[test]
fn para_tets_test() -> Result {
    let in1 = include_bytes!("../assets/para_test.vtk");
    let in2 = include_str!("../assets/para_test_ascii.vtk").as_bytes();
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("vtk output"),
        data: DataSet::UnstructuredGrid {
            points: vec![
                13.2, 135.4, -7.7, 13.7, 134.2, -8.7, 12.2, 134.7, -8.6, 12.7, 133.6, -7.0, 3.6,
                119.4, -0.3, -2.3, 137.0, -2.5, 5.4, 119.7, 0.0, -2.7, 135.9, -1.2, -2.9, 137.5,
                -1.2, -1.8, 136.6, -1.7, 4.3, 119.7, 0.4, 4.6, 118.7, -0.002,
            ]
            .into(),
            cells: Cells {
                num_cells: 3,
                vertices: vec![4, 9, 5, 7, 8, 4, 3, 2, 0, 1, 4, 11, 6, 4, 10],
            },
            cell_types: vec![CellType::Tetra; 3],
            data: Attributes {
                point: vec![(
                    String::from("FieldData"),
                    Attribute::Field {
                        data_array: vec![
                            FieldArray {
                                name: String::from("Zeros"),
                                num_comp: 1,
                                data: vec![0.0f32; 12].into(),
                            },
                            FieldArray {
                                name: String::from("Floats"),
                                num_comp: 1,
                                data: vec![
                                    2.3, 2.5, 2.3, 2.1, 1.4, 0.8, 1.6, 0.7, 0.8, 0.7, 1.5, 1.6f32,
                                ]
                                .into(),
                            },
                            FieldArray {
                                name: String::from("Ints"),
                                num_comp: 1,
                                data: vec![1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0i32].into(),
                            },
                            FieldArray {
                                name: String::from("NegativeInts"),
                                num_comp: 1,
                                data: vec![-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1i32]
                                    .into(),
                            },
                            FieldArray {
                                name: String::from("MixedInts"),
                                num_comp: 1,
                                data: vec![2, -1, 3, -1, -1, -1, -1, -1, -1, 0, -1, 1i32].into(),
                            },
                        ],
                    },
                )],
                cell: vec![(
                    String::from("FieldData"),
                    Attribute::Field {
                        data_array: vec![
                            FieldArray {
                                name: String::from("Ones"),
                                num_comp: 1,
                                data: vec![1.0f32; 3].into(),
                            },
                            FieldArray {
                                name: String::from("Zeros"),
                                num_comp: 1,
                                data: vec![0.0f32; 3].into(),
                            },
                        ],
                    },
                )],
            },
        },
    };
    test_ignore_rem!(parse_be(in1) => out1);
    test_ignore_rem!(parse_be(in2) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn tet_test() -> Result {
    let in1 = include_str!("../assets/tet.vtk");
    let in2 = include_bytes!("../assets/tet_test.vtk");
    let in3 = include_bytes!("../assets/tet_test_binary.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Tetrahedron example"),
        data: DataSet::UnstructuredGrid {
            points: vec![
                0.0f32, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0,
            ]
            .into(),
            cells: Cells {
                num_cells: 1,
                vertices: vec![4, 0, 1, 2, 3],
            },
            cell_types: vec![CellType::Tetra],
            data: Attributes::new(),
        },
    };
    test!(parse_le(in1) => out1);
    test_b!(parse_le(in2) => out1);
    test_b!(parse_be(in3) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn tri_test() -> Result {
    let in1 = include_str!("../assets/tri.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Triangle example"),
        data: DataSet::PolyData {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            topo: vec![PolyDataTopology::Polygons(Cells {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            })],
            data: Attributes::new(),
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn tri_attrib_ascii_test() -> Result {
    let in1 = include_str!("../assets/tri_attrib.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Triangle example"),
        data: DataSet::PolyData {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            topo: vec![PolyDataTopology::Polygons(Cells {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            })],
            data: Attributes {
                point: vec![],
                cell: vec![
                    (
                        String::from("scalars"),
                        Attribute::ColorScalars {
                            num_comp: 3,
                            data: vec![1.0f32, 0.0, 0.0].into(),
                        },
                    ),
                    (
                        String::from("tex_coords"),
                        Attribute::TextureCoordinates {
                            dim: 3,
                            data: vec![1.0f32, 0.0, 0.0].into(),
                        },
                    ),
                    (
                        String::from("tensors"),
                        Attribute::Tensors {
                            data: vec![1.0f64, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0].into(),
                        },
                    ),
                ],
            },
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    // Color scalars are floats only in the ASCII vtk file format. Thus we have another test for
    // The same file but in binary in tri_attrib_binary_test.
    Ok(())
}

#[test]
fn tri_attrib_binary_test() -> Result {
    let in1 = include_bytes!("../assets/tri_attrib_binary.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Triangle example"),
        data: DataSet::PolyData {
            points: vec![0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, -1.0].into(),
            topo: vec![PolyDataTopology::Polygons(Cells {
                num_cells: 1,
                vertices: vec![3, 0, 1, 2],
            })],
            data: Attributes {
                point: vec![],
                cell: vec![
                    (
                        String::from("scalars"),
                        Attribute::ColorScalars {
                            num_comp: 3,
                            data: vec![255u8, 0, 0].into(),
                        },
                    ),
                    (
                        String::from("tex_coords"),
                        Attribute::TextureCoordinates {
                            dim: 3,
                            data: vec![1.0f32, 0.0, 0.0].into(),
                        },
                    ),
                    (
                        String::from("tensors"),
                        Attribute::Tensors {
                            data: vec![1.0f64, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0].into(),
                        },
                    ),
                ],
            },
        },
    };
    test_ignore_rem!(parse_be(in1) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    // Color scalars are u8 only in the Binary vtk file format. ASCII style color scalars are
    // stored as floats and are tested in tri_attrib_ascii_test.
    Ok(())
}

#[test]
fn square_test() -> Result {
    let in1 = include_str!("../assets/square.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Square example"),
        data: DataSet::PolyData {
            points: vec![
                0.0f32, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0, -1.0,
            ]
            .into(),
            topo: vec![PolyDataTopology::Polygons(Cells {
                num_cells: 1,
                vertices: vec![4, 0, 1, 2, 3],
            })],
            data: Attributes::new(),
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn cube_test() -> Result {
    let in1 = include_str!("../assets/cube.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Cube example"),
        data: DataSet::UnstructuredGrid {
            points: vec![
                0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 1.0, -1.0, 1.0, 0.0, 0.0, 1.0,
                0.0, -1.0, 1.0, 1.0, 0.0, 1.0, 1.0, -1.0f32,
            ]
            .into(),
            cells: Cells {
                num_cells: 1,
                vertices: vec![8, 0, 4, 5, 1, 2, 6, 7, 3],
            },
            cell_types: vec![CellType::Hexahedron],
            data: Attributes::new(),
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn structured_grid_test() -> Result {
    let in1 = include_str!("../assets/structured_grid.vtk");
    let out1 = Vtk {
        version: Version::new((3, 0)),
        title: String::from("vtk output"),
        data: DataSet::StructuredGrid {
            dims: [2, 2, 2],
            points: vec![
                0_f32, 0.2, 0., 0.1, 0.184843, 0., 0., 0.25, 0., 0.1, 0.234843, 0., 0., 0.2,
                0.333333, 0.1, 0.184843, 0.333333, 0., 0.25, 0.333333, 0.1, 0.234843, 0.333333,
            ]
            .into(),
            data: Attributes {
                point: vec![
                    (
                        String::from("ptval"),
                        Attribute::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                            data: vec![0f32, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0].into(),
                        },
                    ),
                    (
                        String::from("ptvec"),
                        Attribute::Vectors {
                            data: vec![
                                0_f32, 0.0287671, 0., 0., 0.0258604, 0., 0., 0.0287671, 0., 0.,
                                0.0258604, 0., 0., 0.0287671, 0., 0., 0.0258604, 0., 0., 0.0287671,
                                0., 0., 0.0258604, 0.,
                            ]
                            .into(),
                        },
                    ),
                ],
                cell: vec![
                    (
                        String::from("cellval"),
                        Attribute::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                            data: vec![1489.0f32].into(),
                        },
                    ),
                    (
                        String::from("cellvec"),
                        Attribute::Vectors {
                            data: vec![0.6f32, 0.7, 0.5].into(),
                        },
                    ),
                ],
            },
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn rectilinear_grid_test() -> Result {
    let in1 = include_bytes!("../assets/rectilinear_grid.vtk");
    let out1 = Vtk {
        version: Version::new((3, 0)),
        title: String::from("vtk output"),
        data: DataSet::RectilinearGrid {
            dims: [3, 4, 1],
            x_coords: vec![0_f32, 2.0, 4.0].into(),
            y_coords: vec![1_f32, 2.0, 3.0, 4.0].into(),
            z_coords: vec![0_f32].into(),
            data: Attributes {
                point: vec![],
                cell: vec![(
                    String::from("FieldData"),
                    Attribute::Field {
                        data_array: vec![FieldArray {
                            name: String::from("cellscalar"),
                            num_comp: 1,
                            data: vec![1.1_f32, 7.5, 1.2, 1.5, 2.6, 8.1].into(),
                        }],
                    },
                )],
            },
        },
    };
    test_b!(parse_be(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);

    // Same thing should work in binary
    let in1 = include_bytes!("../assets/rectilinear_grid_binary.vtk");
    let out2 = Vtk {
        version: Version::new((4, 2)),
        ..out1
    };
    test_b!(parse_be(in1) => out2);
    Ok(())
}

#[test]
fn field_test() -> Result {
    let in1 = include_bytes!("../assets/field.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("field example"),
        data: DataSet::Field {
            name: String::from("FieldData"),
            data_array: vec![
                FieldArray {
                    name: String::from("cellIds"),
                    num_comp: 1,
                    data: vec![0, 1, 2, 3, 4, 5].into(),
                },
                FieldArray {
                    name: String::from("faceAttributes"),
                    num_comp: 2,
                    data: vec![0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6.].into(),
                },
            ],
        },
    };
    test_b!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn cube_complex_test() -> Result {
    let mut topo = vec![PolyDataTopology::Polygons(Cells {
        num_cells: 6,
        vertices: vec![
            4, 0, 1, 2, 3, 4, 4, 5, 6, 7, 4, 0, 1, 5, 4, 4, 2, 3, 7, 6, 4, 0, 4, 7, 3, 4, 1, 2, 6,
            5,
        ],
    })];

    let mut attributes = Attributes {
        point: vec![
            (
                String::from("sample_scalars"),
                Attribute::Scalars {
                    num_comp: 1,
                    lookup_table: Some(String::from("my_table")),
                    data: vec![0.0f32, 1., 2., 3., 4., 5., 6., 7.].into(),
                },
            ),
            (
                String::from("my_table"),
                Attribute::LookupTable {
                    data: vec![
                        0.0f32, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0,
                        0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
                        1.0, 1.0, 1.0,
                    ]
                    .into(),
                },
            ),
        ],
        cell: vec![
            (
                String::from("cell_scalars"),
                Attribute::Scalars {
                    num_comp: 1,
                    lookup_table: None,
                    data: vec![0, 1, 2, 3, 4, 5].into(),
                },
            ),
            (
                String::from("cell_normals"),
                Attribute::Normals {
                    data: vec![
                        0.0f32, 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0.,
                        0.,
                    ]
                    .into(),
                },
            ),
            (
                String::from("FieldData"),
                Attribute::Field {
                    data_array: vec![
                        FieldArray {
                            name: String::from("cellIds"),
                            num_comp: 1,
                            data: vec![0, 1, 2, 3, 4, 5].into(),
                        },
                        FieldArray {
                            name: String::from("faceAttributes"),
                            num_comp: 2,
                            data: vec![0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6.].into(),
                        },
                    ],
                },
            ),
        ],
    };
    let points: IOBuffer = vec![
        0.0, 0.0, 0.0, 1.0, 0.0, 0.0f32, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0,
        1.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0,
    ]
    .into();

    let in1 = include_str!("../assets/cube_complex.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Cube example"),
        data: DataSet::PolyData {
            points: points.clone(),
            topo: topo.clone(),
            data: attributes.clone(),
        },
    };

    let in2 = include_bytes!("../assets/cube_complex_topo.vtk");

    topo.push(PolyDataTopology::Vertices(Cells {
        num_cells: 2,
        vertices: vec![2, 0, 1, 2, 2, 3],
    }));

    attributes.cell = vec![
        (
            String::from("cell_scalars"),
            Attribute::Scalars {
                num_comp: 1,
                lookup_table: None,
                data: vec![0, 1, 2, 3, 4, 5, 6, 7].into(),
            },
        ),
        (
            String::from("cell_normals"),
            Attribute::Normals {
                data: vec![
                    0.0f32, 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., -1., 0., 0., 1., 0., 0.,
                    1., 0., 0., 1., 0., 0.,
                ]
                .into(),
            },
        ),
        (
            String::from("FieldData"),
            Attribute::Field {
                data_array: vec![
                    FieldArray {
                        name: String::from("cellIds"),
                        num_comp: 1,
                        data: vec![0, 1, 2, 3, 4, 5, 7, 8].into(),
                    },
                    FieldArray {
                        name: String::from("faceAttributes"),
                        num_comp: 2,
                        data: vec![
                            0.0f32, 1., 1., 2., 2., 3., 3., 4., 4., 5., 5., 6., 6., 6., 7., 7.,
                        ]
                        .into(),
                    },
                ],
            },
        ),
    ];

    let out2 = Vtk {
        data: DataSet::PolyData {
            points: points.clone(),
            topo: topo.clone(),
            data: attributes.clone(),
        },
        ..out1.clone()
    };

    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    test_b!(parse(in2) => out2);
    test_b!(parse(String::new().write_vtk(out2.clone())?.as_bytes()) => out2);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out2.clone())?) => out2);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out2.clone())?) => out2);
    Ok(())
}

#[test]
fn unstructured_grid_complex_test() -> Result {
    let in1 = include_str!("../assets/unstructured_grid_complex.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Unstructured Grid Example"),
        data: DataSet::UnstructuredGrid {
            points: vec![
                0.0f32, 0., 0., 1., 0., 0., 2., 0., 0., 0., 1., 0., 1., 1., 0., 2., 1., 0., 0., 0.,
                1., 1., 0., 1., 2., 0., 1., 0., 1., 1., 1., 1., 1., 2., 1., 1., 0., 1., 2., 1., 1.,
                2., 2., 1., 2., 0., 1., 3., 1., 1., 3., 2., 1., 3., 0., 1., 4., 1., 1., 4., 2., 1.,
                4., 0., 1., 5., 1., 1., 5., 2., 1., 5., 0., 1., 6., 1., 1., 6., 2., 1., 6.,
            ]
            .into(),
            cells: Cells {
                num_cells: 12,
                vertices: vec![
                    10, 0, 1, 4, 3, 6, 7, 10, 9, 2, 5,
                    8, 0, 1, 4, 3, 6, 7, 10, 9, 8, 1, 2, 5, 4, 7, 8, 11, 10, 4, 6, 10, 9, 12, 4, 5,
                    11, 10, 14, 6, 15, 16, 17, 14, 13, 12, 6, 18, 15, 19, 16, 20, 17, 4, 22, 23,
                    20, 19, 3, 21, 22, 18, 3, 22, 19, 18, 2, 26, 25, 1, 24,
                ],
            },
            cell_types: vec![
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
            data: Attributes {
                point: vec![
                    (
                        String::from("scalars"),
                        Attribute::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                            data: vec![
                                0.0f32, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0,
                                12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0,
                                23.0, 24.0, 25.0, 26.0,
                            ]
                            .into(),
                        },
                    ),
                    (
                        String::from("vectors"),
                        Attribute::Vectors {
                            data: vec![
                                1.0f32, 0., 0., 1., 1., 0., 0., 2., 0., 1., 0., 0., 1., 1., 0., 0.,
                                2., 0., 1., 0., 0., 1., 1., 0., 0., 2., 0., 1., 0., 0., 1., 1., 0.,
                                0., 2., 0., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0.,
                                1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0.,
                                0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1.,
                            ]
                            .into(),
                        },
                    ),
                ],
                cell: vec![],
            },
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn volume_complex_test() -> Result {
    let in1 = include_str!("../assets/volume_complex.vtk");
    let out1 = Vtk {
        version: Version::new((2, 0)),
        title: String::from("Volume example"),
        data: DataSet::StructuredPoints {
            dims: [3, 4, 6],
            origin: [0.0, 0.0, 0.0],
            spacing: [1.0, 1.0, 1.0],
            data: Attributes {
                point: vec![(
                    String::from("volume_scalars"),
                    Attribute::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                        data: vec![
                            0i8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 15, 20, 25, 25, 20, 15,
                            10, 5, 0, 0, 10, 20, 30, 40, 50, 50, 40, 30, 20, 10, 0, 0, 10, 20, 30,
                            40, 50, 50, 40, 30, 20, 10, 0, 0, 5, 10, 15, 20, 25, 25, 20, 15, 10, 5,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        ]
                        .into(),
                    },
                )],
                cell: vec![],
            },
        },
    };
    test!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn dodecagon_test() -> Result {
    let in1 = include_bytes!("../assets/dodecagon_ascii_simple.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Dodecagon example"),
        data: DataSet::UnstructuredGrid {
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
                num_cells: 1,
                vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            },
            cell_types: vec![CellType::Polygon],
            data: Attributes::new(),
        },
    };

    test_b!(parse(in1) => out1);
    test_b!(parse(String::new().write_vtk(out1.clone())?.as_bytes()) => out1);
    test_b!(parse(Vec::<u8>::new().write_vtk(out1.clone())?) => out1);
    test_b!(parse_le(Vec::<u8>::new().write_vtk_le(out1.clone())?) => out1);
    test_b!(parse_be(Vec::<u8>::new().write_vtk_be(out1.clone())?) => out1);
    Ok(())
}

#[test]
fn dodecagon_with_meta_test() {
    let in1 = include_bytes!("../assets/dodecagon_ascii.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Dodecagon example"),
        data: DataSet::UnstructuredGrid {
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
                num_cells: 1,
                vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            },
            cell_types: vec![CellType::Polygon],
            data: Attributes::new(),
        },
    };

    test_b!(parse(in1) => out1);
}

#[test]
fn binary_dodecagon_test() {
    let in1 = include_bytes!("../assets/dodecagon_simple.vtk");
    let out1 = Vtk {
        version: Version::new((4, 2)),
        title: String::from("Dodecagon example"),
        data: DataSet::UnstructuredGrid {
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
                num_cells: 1,
                vertices: vec![12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
            },
            cell_types: vec![CellType::Polygon],
            data: Attributes::new(),
        },
    };
    let in2 = include_bytes!("../assets/dodecagon.vtk");

    test_b!(parse_be(in1) => out1);
    test_b!(parse_be(in2) => out1);
}
