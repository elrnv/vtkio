// TODO: migrating to a newer version of nom could reduce the detected "cognitive complexity"
// caused by macros.
#![allow(clippy::cognitive_complexity)]

use std::marker::PhantomData;

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};
use nom::branch::alt;
use nom::bytes::streaming::{is_not, tag, tag_no_case};
use nom::character::streaming::{u8};
use nom::combinator::{complete, fail, map, map_res};
use nom::error::dbg_dmp;
use nom::sequence::{separated_pair, tuple};
use nom::IResult;
use nom::Parser;

pub use crate::basic::parsers::*;
pub use crate::basic::*;
use crate::model::*;

use crate::model::ByteOrder as ByteOrderTag;

/*
enum Axis {
    X,
    Y,
    Z,
}
*/

/**
 * Mesh data parsing.
 */
pub struct VtkParser<BO: ByteOrder>(PhantomData<BO>);

/*
/// Helper struct for parsing polygon topology sections.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum PolyDataTopology {
    Verts = 0,
    Lines,
    Polys,
    Strips,
}
 */

impl<BO: ByteOrder + 'static> VtkParser<BO> {
    fn version(input: &[u8]) -> IResult<&[u8], Version> {
        let (input, _) = tuple((
            sp(tag("#")),
            sp(tag_no_case("vtk")),
            sp(tag_no_case("DataFile")),
            sp(tag_no_case("Version")),
        ))(input)?;
        sp(map(separated_pair(u8, tag("."), u8), Version::new))(input)
    }

    fn title(input: &[u8]) -> IResult<&[u8], &str> {
        map_res(is_not("\r\n"), std::str::from_utf8)(input)
    }

    fn file_type(input: &[u8]) -> IResult<&[u8], FileType> {
        alt((
            map(sp(tag_no_case("ASCII")), |_| FileType::ASCII),
            map(sp(tag_no_case("BINARY")), |_| FileType::Binary),
        ))(input)
    }

    fn header(input: &[u8]) -> IResult<&[u8], (Version, String, FileType)> {
        tuple((
            line(Self::version),
            line(map(Self::title, String::from)),
            line(Self::file_type),
        ))(input)
    }

    fn dataset(input: &[u8], _file_type: FileType) -> IResult<&[u8], DataSet> {
        fail(input)
    }

    /// Parse the entire vtk file
    fn vtk(input: &[u8]) -> IResult<&[u8], Vtk> {
        let p = |input| {
            let (input, h) = Self::header(input)?;
            dbg!(&h);
            let (input, d) = Self::dataset(input, h.2)?;

            Ok((
                input,
                Vtk {
                    version: h.0,
                    // This is ignored in Legacy formats
                    byte_order: ByteOrderTag::new::<BO>(),
                    title: h.1,
                    data: d,
                    file_path: None,
                },
            ))
        };

        complete(p)(input).map_err(|e| {
            //p(input).map_err(|e| {
            if let nom::Err::Error(e) = &e {
                dbg!(std::str::from_utf8(e.input));
            };
            return e;
        })
    }
}

/// Parse the entire VTK file using native endian byte order.
pub fn parse_ne(input: &[u8]) -> IResult<&[u8], Vtk> {
    VtkParser::<NativeEndian>::vtk(input)
}

/// Parse the entire VTK file using little endian byte order.
pub fn parse_le(input: &[u8]) -> IResult<&[u8], Vtk> {
    VtkParser::<LittleEndian>::vtk(input)
}

/// Parse the entire VTK file using big endian byte order.
///
/// This is the default VTK byte order. Binary `.vtk` files produced by ParaView are in big endian
/// form.
pub fn parse_be(input: &[u8]) -> IResult<&[u8], Vtk> {
    VtkParser::<BigEndian>::vtk(input)
}

#[cfg(test)]
mod tests {
    /*
    use super::*;
    use nom::IResult;

    #[test]
    fn file_type_test() {
        let f = file_type("BINARY".as_bytes());
        assert_eq!(f, IResult::Done(&b""[..], FileType::Binary));
        let f = file_type("ASCII".as_bytes());
        assert_eq!(f, IResult::Done(&b""[..], FileType::ASCII));
    }
    #[test]
    fn version_test() {
        let f = version("\n\t# vtk DataFile Version 2.0  \ntitle\n".as_bytes());
        assert_eq!(f, IResult::Done("title\n".as_bytes(), Version::new((2, 0))));
    }
    #[test]
    fn title_test() {
        let f = title("This is a title\nBINARY".as_bytes());
        assert_eq!(f, IResult::Done("BINARY".as_bytes(), "This is a title"));
    }
    #[test]
    fn points_test() {
        let in1 = "POINTS 0 float\n";
        let in2 = "POINTS 3 float\n2 45 2 3 4 1 46 2 0\nother";
        let f = VtkParser::<NativeEndian>::points(in1.as_bytes(), FileType::ASCII);
        assert_eq!(f, IResult::Done("".as_bytes(), Vec::<f32>::new().into()));
        let f = VtkParser::<NativeEndian>::points(in2.as_bytes(), FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "other".as_bytes(),
                vec![2.0f32, 45., 2., 3., 4., 1., 46., 2., 0.].into()
            )
        );
    }
    #[test]
    fn cells_test() {
        let in1 = "CELLS 0 0\n";
        let in2 = "CELLS 1 3\n2 1 2\nother";

        let f = VtkParser::<NativeEndian>::cell_verts(in1.as_bytes(), "CELLS", FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "".as_bytes(),
                VertexNumbers::Legacy {
                    num_cells: 0,
                    vertices: vec![]
                }
            )
        );
        let f = VtkParser::<NativeEndian>::cell_verts(in2.as_bytes(), "CELLS", FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "other".as_bytes(),
                VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![2, 1, 2]
                }
            )
        );
    }
    #[test]
    fn cell_type_test() {
        let f = VtkParser::<NativeEndian>::cell_type("2".as_bytes());
        assert_eq!(f, IResult::Done("".as_bytes(), CellType::PolyVertex));
        let f = VtkParser::<NativeEndian>::cell_type("10".as_bytes());
        assert_eq!(f, IResult::Done("".as_bytes(), CellType::Tetra));
    }

    macro_rules! test {
        ($fn:ident ($in:expr, $($args:expr),*) => ($rem:expr, $out:expr)) => {
            assert_eq!(VtkParser::<NativeEndian>::$fn($in.as_bytes(), $($args),*), IResult::Done($rem.as_bytes(), $out.clone()));
        };
        ($fn:ident ($in:expr) => ($rem:expr, $out:expr)) => {
            assert_eq!(VtkParser::<NativeEndian>::$fn($in.as_bytes()), IResult::Done($rem.as_bytes(), $out.clone()));
        };
        ($fn:ident ($in:expr, $($args:expr),*) => $out:expr) => {
            test!($fn($in, $($args),*) => ("", $out));
        };
        ($fn:ident ($in:expr) => $out:expr) => {
            test!($fn($in) => ("", $out));
        }
    }

    #[test]
    fn cell_types_test() {
        let in1 = "CELL_TYPES 0\nother";
        let out1 = Vec::<CellType>::new();
        let in2 = "CELL_TYPES 3\n2 1 10\nother";
        let out2 = vec![CellType::PolyVertex, CellType::Vertex, CellType::Tetra];
        test!(cell_types(in1, FileType::ASCII) => ("other", out1));
        test!(cell_types(in2, FileType::ASCII) => ("other", out2));
    }

    #[test]
    fn unstructured_grid_test() {
        let in1 = "UNSTRUCTURED_GRID\nPOINTS 4 float\n\
                   2 45 2 3 4 1 46 2 0 4 32 1\nCELLS 2 10\n4 0 1 2 3\n4 3 2 1 0
                   CELL_TYPES 2\n 10 10\nother";
        let out1 = DataSet::inline(UnstructuredGridPiece {
            points: vec![2.0f32, 45., 2., 3., 4., 1., 46., 2., 0., 4., 32., 1.].into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 2,
                    vertices: vec![4, 0, 1, 2, 3, 4, 3, 2, 1, 0],
                },
                types: vec![CellType::Tetra; 2],
            },
            data: Attributes::new(),
        });

        test!(unstructured_grid(in1, FileType::ASCII) => ("other", out1));
    }
    #[test]
    fn attribute_test() {
        // scalar attribute
        let in1 = "SCALARS cell_scalars int 1\n0 1 2 3 4 5";
        let out1 = Attribute::DataArray(DataArray {
            name: String::from("cell_scalars"),
            elem: ElementType::Scalars {
                num_comp: 1,
                lookup_table: None,
            },
            data: vec![0, 1, 2, 3, 4, 5].into(),
        });
        test!(attribute(in1, 6, FileType::ASCII) => ("", out1));
    }
    #[test]
    fn attributes_test() {
        // empty cell attributes
        test!(cell_attributes("\n", FileType::ASCII) => Vec::new());
        // empty point attributes
        test!(point_attributes("", FileType::ASCII) => Vec::new());
        // empty
        test!(attributes("\n", FileType::ASCII) => Attributes::new());
        // scalar cell attribute
        let in1 = "CELL_DATA 6\nSCALARS cell_scalars int 1\n0 1 2 3 4 5\n";
        let scalar_data = DataArray {
            name: String::new(),
            elem: ElementType::Scalars {
                num_comp: 1,
                lookup_table: None,
            },
            data: vec![0, 1, 2, 3, 4, 5].into(),
        };
        let out1 = vec![Attribute::DataArray(DataArray {
            name: String::from("cell_scalars"),
            ..scalar_data.clone()
        })];
        test!(cell_attributes(in1, FileType::ASCII) => out1);
        // scalar point and cell attributes
        let in2 = "POINT_DATA 6\n SCALARS point_scalars int 1\n0 1 2 3 4 5\n
                   CELL_DATA 6\n SCALARS cell_scalars int 1\n0 1 2 3 4 5";
        let pt_res = vec![Attribute::DataArray(DataArray {
            name: String::from("point_scalars"),
            ..scalar_data.clone()
        })];
        let cl_res = vec![Attribute::DataArray(DataArray {
            name: String::from("cell_scalars"),
            ..scalar_data
        })];
        let out2 = Attributes {
            point: pt_res,
            cell: cl_res,
        };
        test!(attributes(in2, FileType::ASCII) => out2);
    }
    #[test]
    fn dataset_simple_test() {
        let in1 = "DATASET UNSTRUCTURED_GRID\nPOINTS 0 float\nCELLS 0 0\nCELL_TYPES 0\n";
        let out1 = DataSet::inline(UnstructuredGridPiece {
            points: Vec::<f32>::new().into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 0,
                    vertices: vec![],
                },
                types: vec![],
            },
            data: Attributes::new(),
        });
        test!(dataset(in1, FileType::ASCII) => out1);
    }
    #[test]
    fn dataset_test() {
        let in1 = "DATASET UNSTRUCTURED_GRID\nPOINTS 3 float\n2 45 2 3 4 1 46 2 0\
                   CELLS 0 0\nCELL_TYPES 0\n";
        let out1 = DataSet::inline(UnstructuredGridPiece {
            points: vec![2.0f32, 45., 2., 3., 4., 1., 46., 2., 0.].into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 0,
                    vertices: vec![],
                },
                types: vec![],
            },
            data: Attributes::new(),
        });
        test!(dataset(in1, FileType::ASCII) => out1);
    }
    #[test]
    fn dataset_crlf_test() {
        let in1 = "DATASET UNSTRUCTURED_GRID\r\nPOINTS 3 float\r\n2 45 2 3 4 1 46 2 0\
                   CELLS 0 0\r\nCELL_TYPES 0\r\n";
        let out1 = DataSet::inline(UnstructuredGridPiece {
            points: vec![2.0f32, 45., 2., 3., 4., 1., 46., 2., 0.].into(),
            cells: Cells {
                cell_verts: VertexNumbers::Legacy {
                    num_cells: 0,
                    vertices: vec![],
                },
                types: vec![],
            },
            data: Attributes::new(),
        });
        test!(dataset(in1, FileType::ASCII) => out1);
    }
    */
}
