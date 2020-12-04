use std::io::BufReader;
use vtkio::{import, model::*, parse_xml, Error};

type Result = std::result::Result<(), Error>;

fn make_box_vtu() -> Vtk {
    Vtk {
        version: Version { major: 4, minor: 2 },
        title: String::new(),
        byte_order: ByteOrder::BigEndian,
        data: DataSet::inline(UnstructuredGridPiece {
            points: IOBuffer::F64(vec![
                0.5208333134651184,
                -0.5,
                0.5,
                -0.5208333134651184,
                -0.5,
                0.5,
                0.5208333134651184,
                0.5,
                0.5,
                -0.5208333134651184,
                0.5,
                0.5,
                -0.5208333134651184,
                -0.5,
                -0.5,
                0.5208333134651184,
                -0.5,
                -0.5,
                -0.5208333134651184,
                0.5,
                -0.5,
                0.5208333134651184,
                0.5,
                -0.5,
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![
                        0, 1, 3, 2, 4, 5, 7, 6, 6, 7, 2, 3, 5, 4, 1, 0, 5, 0, 2, 7, 1, 4, 6, 3,
                    ],
                    offsets: vec![4, 8, 12, 16, 20, 24],
                },
                types: vec![CellType::Polygon; 6],
            },
            data: Attributes {
                point: vec![
                    Attribute::DataArray(DataArrayBase {
                        name: String::from("pressure"),
                        elem: ElementType::Scalars {
                            num_comp: 1,
                            lookup_table: None,
                        },
                        data: IOBuffer::F32(vec![-0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5, 0.5]),
                    }),
                    Attribute::DataArray(DataArrayBase {
                        name: String::from("Cd"),
                        elem: ElementType::Vectors,
                        data: IOBuffer::F32(vec![
                            0.2, 0.0, 1.0, 0.2, 0.0, 1.0, 0.0, 1.0, 0.1, 0.0, 1.0, 0.1, 0.2, 0.0,
                            1.0, 0.2, 0.0, 1.0, 0.0, 1.0, 0.1, 0.0, 1.0, 0.1,
                        ]),
                    }),
                    Attribute::DataArray(DataArrayBase {
                        name: String::from("mtl_id"),
                        elem: ElementType::Generic(1),
                        data: IOBuffer::I32(vec![1, 1, 1, 1, 1, 1, 1, 1]),
                    }),
                ],
                cell: vec![Attribute::DataArray(DataArrayBase {
                    name: String::from("mtl_id"),
                    elem: ElementType::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                    },
                    data: IOBuffer::I32(vec![0, 0, 0, 0, 0, 0]),
                })],
            },
        }),
    }
}

#[test]
fn box_parse_xml() -> Result {
    let input: &[u8] = include_bytes!("../assets/box.vtu");
    let vtk = parse_xml(BufReader::new(input))?;
    assert_eq!(vtk, make_box_vtu());
    Ok(())
}

#[test]
fn box_import() -> Result {
    let vtk = import("./assets/box.vtu")?;
    assert_eq!(vtk, make_box_vtu());
    Ok(())
}

fn make_box_para_vtu() -> Vtk {
    Vtk {
        version: Version { major: 1, minor: 0 },
        title: String::new(),
        byte_order: ByteOrder::LittleEndian,
        data: DataSet::inline(UnstructuredGridPiece {
            points: IOBuffer::F64(vec![
                0.5208333134651184,
                -0.5,
                0.5,
                -0.5208333134651184,
                -0.5,
                0.5,
                0.5208333134651184,
                0.5,
                0.5,
                -0.5208333134651184,
                0.5,
                0.5,
                -0.5208333134651184,
                -0.5,
                -0.5,
                0.5208333134651184,
                -0.5,
                -0.5,
                -0.5208333134651184,
                0.5,
                -0.5,
                0.5208333134651184,
                0.5,
                -0.5,
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![
                        0, 1, 3, 2, 4, 5, 7, 6, 6, 7, 2, 3, 5, 4, 1, 0, 5, 0, 2, 7, 1, 4, 6, 3,
                    ],
                    offsets: vec![4, 8, 12, 16, 20, 24],
                },
                types: vec![CellType::Polygon; 6],
            },
            data: Attributes {
                point: vec![
                    Attribute::scalars("mtl_id", 1)
                        .with_data(IOBuffer::I32(vec![1, 1, 1, 1, 1, 1, 1, 1])),
                    Attribute::DataArray(DataArrayBase {
                        name: String::from("Cd"),
                        elem: ElementType::Vectors,
                        data: IOBuffer::F32(vec![
                            0.2, 0.0, 1.0, 0.2, 0.0, 1.0, 0.0, 1.0, 0.1, 0.0, 1.0, 0.1, 0.2, 0.0,
                            1.0, 0.2, 0.0, 1.0, 0.0, 1.0, 0.1, 0.0, 1.0, 0.1,
                        ]),
                    }),
                    Attribute::generic("pressure", 1).with_data(IOBuffer::F32(vec![
                        -0.5, -0.5, 0.5, 0.5, -0.5, -0.5, 0.5, 0.5,
                    ])),
                ],
                cell: vec![Attribute::DataArray(DataArrayBase {
                    name: String::from("mtl_id"),
                    elem: ElementType::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                    },
                    data: IOBuffer::I32(vec![0, 0, 0, 0, 0, 0]),
                })],
            },
        }),
    }
}

#[test]
fn box_para_parse_xml() -> Result {
    let input: &[u8] = include_bytes!("../assets/box_para.vtu");
    let vtk = parse_xml(BufReader::new(input))?;
    assert_eq!(vtk, make_box_para_vtu());
    Ok(())
}
