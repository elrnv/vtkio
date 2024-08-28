#![cfg(feature = "xml")]
use pretty_assertions::assert_eq;
use std::io::BufReader;
use vtkio::{model::*, Error};

type Result = std::result::Result<(), Error>;

fn init() {
    let _ = env_logger::builder().is_test(true).try_init();
}

fn make_box_vtu() -> Vtk {
    Vtk {
        version: Version::new_xml(4, 2),
        title: String::new(),
        byte_order: ByteOrder::BigEndian,
        file_path: None,
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
    let vtk = Vtk::parse_xml(BufReader::new(input))?;
    assert_eq!(vtk, make_box_vtu());
    Ok(())
}

#[test]
fn box_import() -> Result {
    let mut vtk = Vtk::import("./assets/box.vtu")?;
    vtk.file_path = None; // erase file path before comparison.
    assert_eq!(vtk, make_box_vtu());
    Ok(())
}

fn make_box_para_vtu() -> Vtk {
    Vtk {
        version: Version::new_xml(1, 0),
        title: String::new(),
        byte_order: ByteOrder::LittleEndian,
        file_path: None,
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
    let vtk = Vtk::parse_xml(BufReader::new(input))?;
    assert_eq!(vtk, make_box_para_vtu());
    Ok(())
}

fn make_hexahedron_vtu() -> Vtk {
    Vtk {
        version: Version::new_xml(1, 0),
        title: String::new(),
        byte_order: ByteOrder::LittleEndian,
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            #[rustfmt::skip]
            points: IOBuffer::F32(vec![
                0.0, 0.0, 0.0,
                0.0, 0.0, -1.0,
                0.0, 1.0, 0.0,
                0.0, 1.0, -1.0,
                1.0, 0.0, 0.0,
                1.0, 0.0, -1.0,
                1.0, 1.0, 0.0,
                1.0, 1.0, -1.0
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![0, 4, 5, 1, 2, 6, 7, 3],
                    offsets: vec![8],
                },
                types: vec![CellType::Hexahedron; 1],
            },
            data: Attributes {
                point: vec![],
                cell: vec![],
            },
        }),
    }
}

#[test]
fn hexahedron_appended() -> Result {
    let mut vtu = Vtk::import("./assets/hexahedron.vtu")?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    Ok(())
}

#[cfg(feature = "binary")]
#[test]
fn hexahedron_pvtu() -> Result {
    let mut vtu = Vtk::import("./assets/hexahedron_parallel.pvtu")?;
    vtu.load_all_pieces().unwrap();
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    Ok(())
}

#[cfg(feature = "liblzma")]
#[test]
fn hexahedron_lzma_pvtu() -> Result {
    let mut vtu = Vtk::import("./assets/hexahedron_parallel_lzma.pvtu")?;
    vtu.load_all_pieces().unwrap();
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    Ok(())
}

#[cfg(feature = "flate2")]
#[test]
fn hexahedron_zlib() -> Result {
    let mut vtu = Vtk::import("./assets/hexahedron_zlib_para.vtu")?;
    vtu.clone()
        .try_into_xml_format(vtkio::xml::Compressor::ZLib, 1)
        .unwrap();
    // vtkfile.export("./assets/hexahedron_zlib_out.vtu").unwrap();
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    Ok(())
}

#[cfg(feature = "binary")]
#[cfg(feature = "flate2")]
#[test]
fn hexahedron_zlib_binary() -> Result {
    let path = "./assets/hexahedron_zlib_binary.vtu";
    let mut vtu = Vtk::import(path)?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    // TODO: Implement a config for converting to XML files and add the following round trip test.
    // Write back into a vtkfile to check compression consistency.
    //let vtkfile = vtu
    //    .clone()
    //    .try_into_xml_format(vtkio::xml::Compressor::ZLib, 9)
    //    .unwrap();
    //let orig_vtkfile = vtkio::xml::VTKFile::import(path)?;
    //assert_eq!(vtkfile, orig_vtkfile); // Checks that compression is the same.
    Ok(())
}

#[cfg(feature = "flate2")]
#[test]
fn hexahedron_zlib_inline_binary() -> Result {
    init();
    let path = "./assets/hexahedron_zlib_inline_binary.vtu";
    let mut vtu = Vtk::import(path)?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    // Write back into a vtkfile to check compression consistency.
    let vtkfile = vtu
        .clone()
        .try_into_xml_format(vtkio::xml::Compressor::ZLib, 6)
        .unwrap();
    // vtkfile.export("./assets/hexahedron_zlib_inline_binary_alt.vtu")?;
    let orig_vtkfile = vtkio::xml::VTKFile::import(path)?;
    assert_eq!(vtkfile, orig_vtkfile); // Checks that compression is the same.
    Ok(())
}

#[cfg(feature = "lz4")]
#[test]
fn hexahedron_lz4() -> Result {
    init();
    let path = "./assets/hexahedron_lz4_inline_binary.vtu";
    let mut vtu = Vtk::import(path)?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    // Write back into a vtkfile to check compression consistency.
    let vtkfile = vtu
        .clone()
        .try_into_xml_format(vtkio::xml::Compressor::LZ4, 9)
        .unwrap();
    // vtkfile.export("./assets/hexahedron_lz4_inline_binary_alt.vtu")?;
    let orig_vtkfile = vtkio::xml::VTKFile::import(path)?;
    assert_eq!(vtkfile, orig_vtkfile); // Checks that compression is the same.
    Ok(())
}

#[cfg(feature = "liblzma")]
#[test]
fn hexahedron_lzma_inline_binary() -> Result {
    init();
    let path = "./assets/hexahedron_lzma_inline_binary.vtu";
    let mut vtu = Vtk::import(path)?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    // Write back into a vtkfile to check compression consistency.
    let vtkfile = vtu
        .clone()
        .try_into_xml_format(vtkio::xml::Compressor::LZMA, 9)
        .unwrap();
    // vtkfile.export("./assets/hexahedron_lzma_inline_binary_alt.vtu")?;
    let orig_vtkfile = vtkio::xml::VTKFile::import(path)?;
    assert_eq!(vtkfile, orig_vtkfile); // Checks that compression is the same.
    Ok(())
}

#[test]
fn point_cloud() -> Result {
    init();
    let vtk = Vtk {
        version: Version::new_xml(1, 0),
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
    let path = "./assets/point_cloud.vtp";
    let orig_vtkfile = vtkio::xml::VTKFile::import(path)?;
    assert_eq!(
        vtk.try_into_xml_format(vtkio::xml::Compressor::None, 0)?,
        orig_vtkfile
    ); // Checks that compression is the same.
    Ok(())
}

#[cfg(feature = "binary")]
#[test]
fn hexahedron_binary() -> Result {
    let mut vtu = Vtk::import("./assets/hexahedron_binary.vtu")?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_hexahedron_vtu());
    Ok(())
}

#[cfg(feature = "binary")]
fn make_tet_vtu() -> Vtk {
    Vtk {
        version: Version::new_xml(1, 0),
        title: String::new(),
        byte_order: ByteOrder::LittleEndian,
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            #[rustfmt::skip]
            points: IOBuffer::F64(vec![
		0.0, 1.0, 0.0,
		-0.9428102970123291, -0.3333297073841095, 0.0,
		0.47140514850616455, -0.3333297073841095, 0.8164976239204407,
		0.47140514850616455, -0.3333297073841095, -0.8164976239204407,
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![3, 1, 0, 2],
                    offsets: vec![4],
                },
                types: vec![CellType::Tetra; 1],
            },
            data: Attributes {
                point: vec![Attribute::DataArray(DataArrayBase {
                    name: String::from("pressure"),
                    elem: ElementType::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                    },
                    data: IOBuffer::F32(vec![0.0, -0.9428103, 0.47140515, 0.47140515]),
                })],
                cell: vec![Attribute::DataArray(DataArrayBase {
                    name: String::from("mtl_id"),
                    elem: ElementType::Scalars {
                        num_comp: 1,
                        lookup_table: None,
                    },
                    data: IOBuffer::I32(vec![1]),
                })],
            },
        }),
    }
}

#[cfg(feature = "binary")]
#[test]
fn single_tet_vtu() -> Result {
    let mut vtu = Vtk::import("./assets/tet.vtu")?;
    vtu.file_path = None; // Reset file path to satisfy comparison
    assert_eq!(vtu, make_tet_vtu());
    Ok(())
}
