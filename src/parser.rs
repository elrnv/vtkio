use std::marker::PhantomData;

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};
use nom::branch::{alt, permutation};
use nom::bytes::complete::{is_not, tag, tag_no_case};
use nom::character::complete::{line_ending, multispace0, u32 as parse_u32, u8 as parse_u8};
use nom::combinator::{complete, cut, eof, flat_map, map, map_opt, map_res, opt};
use nom::multi::{many0, many_m_n};
use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
use nom::IResult;
use num_traits::FromPrimitive;

use crate::basic::*;
use crate::model::ByteOrder as ByteOrderTag;
use crate::model::*;

/*
 * Parsing routines
 */

fn version(input: &[u8]) -> IResult<&[u8], Version> {
    let (input, _) = tuple((
        sp(tag("#")),
        sp(tag_no_case("vtk")),
        sp(tag_no_case("DataFile")),
        sp(tag_no_case("Version")),
    ))(input)?;
    sp(map(
        separated_pair(parse_u32, tag("."), parse_u32),
        |(major, minor)| Version::new_legacy(major, minor),
    ))(input)
}

fn file_type(input: &[u8]) -> IResult<&[u8], FileType> {
    alt((
        map(sp(tag_no_case("ASCII")), |_| FileType::Ascii),
        map(sp(tag_no_case("BINARY")), |_| FileType::Binary),
    ))(input)
}

fn title(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(is_not("\r\n"), std::str::from_utf8)(input)
}

fn header(input: &[u8]) -> IResult<&[u8], (Version, String, FileType)> {
    let (input, _) = multispace0(input)?;
    terminated(
        tuple((
            line(version),
            line(map(title, String::from)),
            line(file_type),
        )),
        multispace0,
    )(input)
}

fn data_type(input: &[u8]) -> IResult<&[u8], ScalarType> {
    alt((
        map(tag_no_case("bit"), |_| ScalarType::Bit),
        map(tag_no_case("int"), |_| ScalarType::I32),
        map(tag_no_case("char"), |_| ScalarType::I8),
        map(tag_no_case("long"), |_| ScalarType::I64),
        map(tag_no_case("short"), |_| ScalarType::I16),
        map(tag_no_case("float"), |_| ScalarType::F32),
        map(tag_no_case("double"), |_| ScalarType::F64),
        map(tag_no_case("unsigned_int"), |_| ScalarType::U32),
        map(tag_no_case("unsigned_char"), |_| ScalarType::U8),
        map(tag_no_case("unsigned_long"), |_| ScalarType::U64),
        map(tag_no_case("vtkIdType"), |_| ScalarType::I32),
    ))(input)
}

fn name(input: &[u8]) -> IResult<&[u8], &str> {
    map_res(is_not(" \t\r\n"), std::str::from_utf8)(input)
}

/// Recognize and throw away `METADATA` block. Metadata is separated by an empty line.
fn meta(input: &[u8]) -> IResult<&[u8], ()> {
    tagged_block(
        tag_no_case("METADATA"),
        map(
            alt((
                eof,
                complete(|mut input| {
                    // Loop until an empty line or eof is found (should be more efficient than
                    // `alt(take_until(), take_until()))`
                    loop {
                        // Consume everything until and including the next line ending
                        (input, _) = preceded(opt(is_not("\n\r")), line_ending)(input)?;
                        match alt((eof, line_ending))(input) {
                            k @ Ok(_) => return k,
                            e @ Err(nom::Err::Failure(_) | nom::Err::Incomplete(_)) => _ = e?,
                            _ => {}
                        };
                    }
                }),
            )),
            |_| (),
        ),
    )(input)
}

/// Parses an array of three values preceded by a tag. E.g. "DIMENSIONS 1 2 3"
fn array3<'a, T: FromAscii>(
    tag: &'static str,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], [T; 3]> {
    preceded(
        sp(tag_no_case(tag)),
        cut(map(
            tuple((sp(T::from_ascii), sp(T::from_ascii), sp(T::from_ascii))),
            |(nx, ny, nz)| [nx, ny, nz],
        )),
    )
}

/// Helper struct for parsing polygon topology sections.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum PolyDataTopology {
    Verts = 0,
    Lines,
    Polys,
    Strips,
}

/**
 * Mesh data parsing.
 */
pub struct VtkParser<BO: ByteOrder>(PhantomData<BO>);

impl<BO: ByteOrder + 'static> VtkParser<BO> {
    fn points(input: &[u8], ft: FileType) -> IResult<&[u8], IOBuffer> {
        tagged_block(tag_no_case("POINTS"), |input| {
            let (input, (n, d_t)) = line(tuple((sp(parse_u32), sp(data_type))))(input)?;

            match d_t {
                ScalarType::F32 => parse_data_buffer::<f32, BO>(input, 3 * n as usize, ft),
                ScalarType::F64 => parse_data_buffer::<f64, BO>(input, 3 * n as usize, ft),
                _ => Err(nom::Err::Error(nom::error::make_error(
                    input,
                    nom::error::ErrorKind::Switch,
                ))),
            }
        })(input)
    }

    /// Parse cell topology indices in the legacy way.
    ///
    /// Cells are stored as a single contiguous array with the format `n v0 v1 ... vn` for each cell.
    fn legacy_cell_topo(
        input: &[u8],
        n: u32,
        size: u32,
        ft: FileType,
    ) -> IResult<&[u8], VertexNumbers> {
        map(
            |i| parse_data_vec::<u32, BO>(i, size as usize, ft),
            |data| VertexNumbers::Legacy {
                num_cells: n,
                vertices: data,
            },
        )(input)
    }

    /// Parse cell topology indices in modern way using offsets and connectivity arras.
    ///
    /// Cells are stored as two arrays: OFFSETS and CONNECTIVITY, which are specified separately.
    fn modern_cell_topo(
        input: &[u8],
        n: u32,
        size: u32,
        ft: FileType,
    ) -> IResult<&[u8], VertexNumbers> {
        map(
            tuple((
                |i| Self::topo(i, "OFFSETS", n, ft),
                |i| Self::topo(i, "CONNECTIVITY", size, ft),
            )),
            |(offsets, connectivity)| VertexNumbers::XML {
                offsets,
                connectivity,
            },
        )(input)
    }

    /// Parse either a CONNECTIVITY or OFFSETS array for modern topology.
    fn topo<'a>(
        input: &'a [u8],
        tag: &'static str,
        n: u32,
        ft: FileType,
    ) -> IResult<&'a [u8], Vec<u64>> {
        tagged_block(
            line(pair(tag_no_case(tag), sp(tag_no_case("vtktypeint64")))),
            |input| parse_data_vec::<u64, BO>(input, n as usize, ft),
        )(input)
    }

    /// Parse a collection of cells. The given tag should be one of
    ///  * "CELLS"
    ///  * "VERTICES"
    ///  * "LINES"
    ///  * "POLYGONS"
    ///  * "TRIANGLE_STRIPS"
    fn cell_verts<'a>(
        input: &'a [u8],
        tag: &'static str,
        ft: FileType,
    ) -> IResult<&'a [u8], VertexNumbers> {
        tagged_block(tag_no_case(tag), |input| {
            let (input, (n, size)) = line(tuple((sp(parse_u32), sp(parse_u32))))(input)?;
            alt((
                move |i| Self::modern_cell_topo(i, n, size, ft),
                move |i| Self::legacy_cell_topo(i, n, size, ft),
            ))(input)
        })(input)
    }

    fn coordinates<'a>(
        input: &'a [u8],
        axis_tag: &'static str,
        ft: FileType,
    ) -> IResult<&'a [u8], IOBuffer> {
        tagged_block(tag_no_case(axis_tag), |input| {
            let (input, (n, dt)) = line(pair(sp(parse_u32), sp(data_type)))(input)?;
            match dt {
                ScalarType::F32 => parse_data_buffer::<f32, BO>(input, n as usize, ft),
                ScalarType::F64 => parse_data_buffer::<f64, BO>(input, n as usize, ft),
                _ => Err(nom::Err::Error(nom::error::make_error(
                    input,
                    nom::error::ErrorKind::Switch,
                ))),
            }
        })(input)
    }

    /**
     * Attribute Parsing
     */

    fn attribute_data(
        input: &[u8],
        n: usize,
        data_type: ScalarType,
        ft: FileType,
    ) -> IResult<&[u8], IOBuffer> {
        match data_type {
            ScalarType::Bit => parse_data_bit_buffer(input, n, ft),
            ScalarType::U8 => parse_data_buffer_u8(input, n, ft),
            ScalarType::I8 => parse_data_buffer_i8(input, n, ft),
            ScalarType::U16 => parse_data_buffer::<u16, BO>(input, n, ft),
            ScalarType::I16 => parse_data_buffer::<i16, BO>(input, n, ft),
            ScalarType::U32 => parse_data_buffer::<u32, BO>(input, n, ft),
            ScalarType::I32 => parse_data_buffer::<i32, BO>(input, n, ft),
            ScalarType::U64 => parse_data_buffer::<u64, BO>(input, n, ft),
            ScalarType::I64 => parse_data_buffer::<i64, BO>(input, n, ft),
            ScalarType::F32 => parse_data_buffer::<f32, BO>(input, n, ft),
            ScalarType::F64 => parse_data_buffer::<f64, BO>(input, n, ft),
        }
    }

    fn attribute_scalars(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        tagged_block(tag_no_case("SCALARS"), |input| {
            let (input, (aname, dt, num_comp)) =
                line(tuple((sp(name), sp(data_type), opt(parse_u32))))(input)?;
            let (input, lookup_tbl_name) =
                opt(line(preceded(sp(tag_no_case("LOOKUP_TABLE")), sp(name))))(input)?;
            let (input, data) =
                Self::attribute_data(input, num_comp.unwrap_or(1) as usize * num_elements, dt, ft)?;
            let (input, _) = opt(meta)(input)?;

            Ok((
                input,
                Attribute::DataArray(DataArray {
                    name: String::from(aname),
                    elem: ElementType::Scalars {
                        num_comp: num_comp.unwrap_or(1),
                        lookup_table: lookup_tbl_name
                            .and_then(|n| (n != "default").then(|| String::from(n))),
                    },
                    data,
                }),
            ))
        })(input)
    }

    fn attribute_lookup_table(input: &[u8], ft: FileType) -> IResult<&[u8], Attribute> {
        tagged_block(tag_no_case("LOOKUP_TABLE"), |input| {
            let (input, (name, num_elements)) = line(tuple((sp(name), sp(parse_u32))))(input)?;
            let (input, data) =
                Self::attribute_data(input, 4 * num_elements as usize, ScalarType::F32, ft)?;
            let (input, _) = opt(meta)(input)?;

            Ok((
                input,
                Attribute::DataArray(DataArray {
                    name: String::from(name),
                    elem: ElementType::LookupTable,
                    data,
                }),
            ))
        })(input)
    }

    /// Helper to `attribute_color_scalars`. This function calls the appropriate data parser for color
    /// scalars.
    fn attribute_color_scalars_data(
        input: &[u8],
        n: usize,
        ft: FileType,
    ) -> IResult<&[u8], IOBuffer> {
        match ft {
            FileType::Ascii => Self::attribute_data(input, n, ScalarType::F32, ft),
            FileType::Binary => Self::attribute_data(input, n, ScalarType::U8, ft),
        }
    }

    fn attribute_color_scalars(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        tagged_block(tag_no_case("COLOR_SCALARS"), |input| {
            let (input, (name, num_comp)) = line(tuple((sp(name), sp(parse_u32))))(input)?;
            let (input, data) =
                Self::attribute_color_scalars_data(input, num_comp as usize * num_elements, ft)?;
            let (input, _) = opt(meta)(input)?;
            Ok((
                input,
                Attribute::DataArray(DataArray {
                    name: String::from(name),
                    elem: ElementType::ColorScalars(num_comp),
                    data,
                }),
            ))
        })(input)
    }

    fn attribute_vectors(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        Self::attribute_fixed_dim(input, "VECTORS", 3, ElementType::Vectors, num_elements, ft)
    }

    fn attribute_normals(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        Self::attribute_fixed_dim(input, "NORMALS", 3, ElementType::Normals, num_elements, ft)
    }

    fn attribute_tex_coords(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        tagged_block(tag_no_case("TEXTURE_COORDINATES"), |input| {
            let (input, (name, dim, dt)) =
                line(tuple((sp(name), sp(parse_u32), sp(data_type))))(input)?;
            let (input, data) = Self::attribute_data(input, dim as usize * num_elements, dt, ft)?;
            let (input, _) = opt(meta)(input)?;
            Ok((
                input,
                Attribute::DataArray(DataArray {
                    name: String::from(name),
                    elem: ElementType::TCoords(dim),
                    data,
                }),
            ))
        })(input)
    }

    fn attribute_tensors(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        Self::attribute_fixed_dim(input, "TENSORS", 9, ElementType::Tensors, num_elements, ft)
    }

    fn attribute_field_array(input: &[u8], ft: FileType) -> IResult<&[u8], FieldArray> {
        let (input, (name, num_comp, num_tuples, dt)) = line(preceded(
            multispace0,
            tuple((sp(name), sp(parse_u32), sp(parse_u32), sp(data_type))),
        ))(input)?;
        let (input, data) =
            Self::attribute_data(input, (num_comp as usize) * (num_tuples as usize), dt, ft)?;
        let (input, _) = opt(meta)(input)?;
        Ok((
            input,
            FieldArray {
                name: String::from(name),
                elem: num_comp,
                data,
            },
        ))
    }

    fn attribute_field(input: &[u8], ft: FileType) -> IResult<&[u8], Attribute> {
        tagged_block(tag_no_case("FIELD"), |input| {
            let (input, (name, n)) = line(tuple((sp(name), sp(parse_u32))))(input)?;
            let (input, data_array) = many_m_n(n as usize, n as usize, |i| {
                Self::attribute_field_array(i, ft)
            })(input)?;

            Ok((
                input,
                (Attribute::Field {
                    name: String::from(name),
                    data_array,
                }),
            ))
        })(input)
    }

    fn attribute_fixed_dim<'a>(
        input: &'a [u8],
        tag: &'static str,
        dim: usize,
        elem: ElementType,
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&'a [u8], Attribute> {
        tagged_block(tag_no_case(tag), |input| {
            let (input, (name, dt)) = line(tuple((sp(name), sp(data_type))))(input)?;
            let (input, data) = Self::attribute_data(input, dim * num_elements, dt, ft)?;
            let (input, _) = opt(meta)(input)?;
            Ok((
                input,
                Attribute::DataArray(DataArray {
                    name: String::from(name),
                    elem: elem.clone(),
                    data,
                }),
            ))
        })(input)
    }

    fn attribute(input: &[u8], num_elements: usize, ft: FileType) -> IResult<&[u8], Attribute> {
        alt((
            |i| Self::attribute_scalars(i, num_elements, ft),
            |i| Self::attribute_color_scalars(i, num_elements, ft),
            |i| Self::attribute_lookup_table(i, ft),
            |i| Self::attribute_vectors(i, num_elements, ft),
            |i| Self::attribute_normals(i, num_elements, ft),
            |i| Self::attribute_tex_coords(i, num_elements, ft),
            |i| Self::attribute_tensors(i, num_elements, ft),
            |i| Self::attribute_field(i, ft),
        ))(input)
    }

    /// Parse CELL_DATA and POINT_DATA attributes
    fn point_or_cell_attributes<'a>(
        input: &'a [u8],
        tag: &'static str,
        ft: FileType,
    ) -> IResult<&'a [u8], Vec<Attribute>> {
        alt((
            tagged_block(
                tag_no_case(tag),
                flat_map(sp(parse_u32), |n| {
                    many0(move |i| Self::attribute(i, n as usize, ft))
                }),
            ),
            map(ws(eof), |_| Vec::new()),
        ))(input)
    }

    /// Parse all DataSet attributes
    fn attributes(input: &[u8], ft: FileType) -> IResult<&[u8], Attributes> {
        map(
            tuple((
                opt(|i| Self::point_or_cell_attributes(i, "CELL_DATA", ft)),
                opt(|i| Self::point_or_cell_attributes(i, "POINT_DATA", ft)),
                opt(|i| Self::point_or_cell_attributes(i, "CELL_DATA", ft)),
            )),
            |(c1, p, c2)| Attributes {
                point: p.unwrap_or_default(),
                cell: if let Some(c) = c1 {
                    c
                } else {
                    c2.unwrap_or_default()
                },
            },
        )(input)
    }

    /// Parse STRUCTURED_POINTS type dataset.
    fn structured_points(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        tagged_block(tag_no_case_line("STRUCTURED_POINTS"), |input| {
            let (input, parms) = permutation((
                line(array3("DIMENSIONS")),
                line(array3("ORIGIN")),
                line(alt((array3("SPACING"), array3("ASPECT_RATIO")))),
            ))(input)?;

            let (input, data) = Self::attributes(input, ft)?;
            Ok((
                input,
                DataSet::ImageData {
                    extent: Extent::Dims(parms.0),
                    origin: parms.1,
                    spacing: parms.2,
                    meta: None,
                    pieces: vec![Piece::Inline(Box::new(ImageDataPiece {
                        extent: Extent::Dims(parms.0),
                        data,
                    }))],
                },
            ))
        })(input)
    }

    /// Parse STRUCTURED_GRID type dataset
    fn structured_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        tagged_block(tag_no_case_line("STRUCTURED_GRID"), |input| {
            let (input, dims) = line(array3("DIMENSIONS"))(input)?;
            let (input, points) = Self::points(input, ft)?;
            let (input, _) = opt(meta)(input)?;
            let (input, data) = Self::attributes(input, ft)?;

            Ok((
                input,
                DataSet::inline(StructuredGridPiece {
                    extent: Extent::Dims(dims),
                    points,
                    data,
                }),
            ))
        })(input)
    }

    /// Parse RECTILINEAR_GRID type dataset
    fn rectilinear_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        tagged_block(tag_no_case_line("RECTILINEAR_GRID"), |input| {
            let (input, dims) = line(array3("DIMENSIONS"))(input)?;
            let (input, (x, y, z)) = tuple((
                |i| Self::coordinates(i, "X_COORDINATES", ft),
                |i| Self::coordinates(i, "Y_COORDINATES", ft),
                |i| Self::coordinates(i, "Z_COORDINATES", ft),
            ))(input)?;
            let (input, data) = Self::attributes(input, ft)?;
            let (input, _) = opt(meta)(input)?;

            Ok((
                input,
                DataSet::inline(RectilinearGridPiece {
                    extent: Extent::Dims(dims),
                    coords: Coordinates { x, y, z },
                    data,
                }),
            ))
        })(input)
    }

    /// Parse FIELD type dataset
    fn field_data(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        Self::attribute_field(input, ft).map(|(input, field)| {
            if let Attribute::Field { name, data_array } = field {
                (input, DataSet::Field { name, data_array })
            } else {
                unreachable!("attribute_field should always return an Attribute::Field");
            }
        })
    }

    /// Parse a single ASCII cell type value. Essentially a byte converted to `CellType` enum.
    fn cell_type(input: &[u8]) -> IResult<&[u8], CellType> {
        map_opt(parse_u8, CellType::from_u8)(input)
    }

    /// Parse a single binary cell type value. Essentially a byte converted to `CellType` enum.
    fn cell_type_binary(input: &[u8]) -> IResult<&[u8], CellType> {
        map_opt(i32::from_binary::<BO>, |x| CellType::from_u8(x as u8))(input)
    }

    fn cell_type_data(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<CellType>> {
        match ft {
            FileType::Ascii => many_m_n(n, n, ws(Self::cell_type))(input),
            FileType::Binary => many_m_n(n, n, Self::cell_type_binary)(input),
        }
    }

    /// Parse cell types for unstructured grids
    fn cell_types(input: &[u8], ft: FileType) -> IResult<&[u8], Vec<CellType>> {
        tagged_block(tag_no_case("CELL_TYPES"), |input| {
            let (input, n) = line(sp(parse_u32))(input)?;
            Self::cell_type_data(input, n as usize, ft)
        })(input)
    }

    /// Parse UNSTRUCTURED_GRID type dataset
    fn unstructured_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        tagged_block(tag_no_case_line("UNSTRUCTURED_GRID"), |input| {
            let (input, p) = Self::points(input, ft)?;
            let (input, _) = opt(meta)(input)?;
            let (input, cell_verts) = Self::cell_verts(input, "CELLS", ft)?;
            let (input, types) = Self::cell_types(input, ft)?;
            let (input, data) = Self::attributes(input, ft)?;

            Ok((
                input,
                DataSet::inline(UnstructuredGridPiece {
                    points: p,
                    cells: Cells { cell_verts, types },
                    data,
                }),
            ))
        })(input)
    }

    /// Parse PolyData topology
    fn poly_data_topo(
        input: &[u8],
        ft: FileType,
    ) -> IResult<&[u8], (PolyDataTopology, VertexNumbers)> {
        alt((
            map(
                |i| Self::cell_verts(i, "LINES", ft),
                |x| (PolyDataTopology::Lines, x),
            ),
            map(
                |i| Self::cell_verts(i, "POLYGONS", ft),
                |x| (PolyDataTopology::Polys, x),
            ),
            map(
                |i| Self::cell_verts(i, "VERTICES", ft),
                |x| (PolyDataTopology::Verts, x),
            ),
            map(
                |i| Self::cell_verts(i, "TRIANGLE_STRIPS", ft),
                |x| (PolyDataTopology::Strips, x),
            ),
        ))(input)
    }

    /// Parse POLYDATA type dataset
    fn poly_data(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        tagged_block(tag_no_case_line("POLYDATA"), |input| {
            let (input, points) = Self::points(input, ft)?;
            let (input, _) = opt(meta)(input)?;
            let (input, topo1) = opt(|i| Self::poly_data_topo(i, ft))(input)?;
            let (input, topo2) = opt(|i| Self::poly_data_topo(i, ft))(input)?;
            let (input, topo3) = opt(|i| Self::poly_data_topo(i, ft))(input)?;
            let (input, topo4) = opt(|i| Self::poly_data_topo(i, ft))(input)?;
            let (input, data) = Self::attributes(input, ft)?;

            // Avoid clones of the topology data by "taking" them out of the (unordered) options
            let mut topos = [topo1, topo2, topo3, topo4];
            let take_topo = |topos: &mut [Option<(PolyDataTopology, VertexNumbers)>], topo_type| {
                topos
                    .iter()
                    .position(|x| matches!(x, Some((tt, _)) if tt == &topo_type))
                    .and_then(|pos| topos[pos].take().map(|(_, topo)| topo))
            };

            Ok((
                input,
                DataSet::inline(PolyDataPiece {
                    points,
                    verts: take_topo(&mut topos, PolyDataTopology::Verts),
                    lines: take_topo(&mut topos, PolyDataTopology::Lines),
                    polys: take_topo(&mut topos, PolyDataTopology::Polys),
                    strips: take_topo(&mut topos, PolyDataTopology::Strips),
                    data,
                }),
            ))
        })(input)
    }

    fn dataset(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        alt((
            tagged_block(
                tag_no_case("DATASET"),
                alt((
                    |i| Self::poly_data(i, ft),
                    |i| Self::structured_grid(i, ft),
                    |i| Self::rectilinear_grid(i, ft),
                    |i| Self::structured_points(i, ft),
                    |i| Self::unstructured_grid(i, ft),
                )),
            ),
            |i| Self::field_data(i, ft),
        ))(input)
    }

    /// Parse the entire VTK file
    fn vtk(input: &[u8]) -> IResult<&[u8], Vtk> {
        complete(|input| {
            let (input, h) = header(input)?;
            let (input, d) = Self::dataset(input, h.2)?;
            // Ignore all trailing spaces and newlines
            let (input, _) = multispace0(input)?;

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
        })(input)
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
    use super::*;

    #[test]
    fn file_type_test() {
        let f = file_type("BINARY".as_bytes());
        assert_eq!(f, Ok((&b""[..], FileType::Binary)));
        let f = file_type("ASCII".as_bytes());
        assert_eq!(f, Ok((&b""[..], FileType::Ascii)));
    }
    #[test]
    fn version_test() {
        let f = version("# vtk DataFile Version 2.0  \ntitle\n".as_bytes());
        assert_eq!(f, Ok(("\ntitle\n".as_bytes(), Version::new_legacy(2, 0))));
    }
    #[test]
    fn title_test() {
        let f = title("This is a title\nBINARY".as_bytes());
        assert_eq!(f, Ok(("\nBINARY".as_bytes(), "This is a title")));
    }
    #[test]
    fn header_test() {
        let f = header(" \t\n# vtk DataFile Version 2.0  \nThis is a title\nBINARY\n".as_bytes());
        assert_eq!(
            f,
            Ok((
                "".as_bytes(),
                (
                    Version::new_legacy(2, 0),
                    "This is a title".to_string(),
                    FileType::Binary
                )
            ))
        );
    }
    #[test]
    fn meta_test() {
        assert_eq!(meta("METADATA".as_bytes()), Ok(("".as_bytes(), ())));
        assert_eq!(meta(" \tMETADATA".as_bytes()), Ok(("".as_bytes(), ())));
        assert_eq!(meta("METADATA\n".as_bytes()), Ok(("".as_bytes(), ())));
        assert_eq!(meta("METADATA\n\n".as_bytes()), Ok(("".as_bytes(), ())));
        assert_eq!(
            meta("METADATA\n\nPOLYGONS 1 4".as_bytes()),
            Ok(("POLYGONS 1 4".as_bytes(), ()))
        );
        assert_eq!(
            meta("METADATA\nINFORMATION 2\nNAME L2_NORM_RANGE LOCATION vtkDataArray\nDATA 2 0.865742 1.73177\n".as_bytes()),
            Ok(("".as_bytes(), ()))
        );
        assert_eq!(
            meta("METADATA\nINFORMATION 2\nNAME L2_NORM_RANGE LOCATION vtkDataArray\nDATA 2 0.865742 1.73177\n\nPOLYGONS 1 4".as_bytes()),
            Ok(("POLYGONS 1 4".as_bytes(), ()))
        );
    }
    #[test]
    fn points_test() {
        let in1 = "POINTS 0 float\n";
        let in2 = "POINTS 3 float\n2 45 2 3 4 1 46 2 0\nother";
        let f = VtkParser::<NativeEndian>::points(in1.as_bytes(), FileType::Ascii);
        assert_eq!(f, Ok(("".as_bytes(), Vec::<f32>::new().into())));
        let f = VtkParser::<NativeEndian>::points(in2.as_bytes(), FileType::Ascii);
        assert_eq!(
            f,
            Ok((
                "other".as_bytes(),
                vec![2.0f32, 45., 2., 3., 4., 1., 46., 2., 0.].into()
            ))
        );
    }
    #[test]
    fn cells_test() {
        let in1 = "CELLS 0 0\n";
        let in2 = "CELLS 1 3\n2 1 2\nother";

        let f = VtkParser::<NativeEndian>::cell_verts(in1.as_bytes(), "CELLS", FileType::Ascii);
        assert_eq!(
            f,
            Ok((
                "".as_bytes(),
                VertexNumbers::Legacy {
                    num_cells: 0,
                    vertices: vec![]
                }
            ))
        );
        let f = VtkParser::<NativeEndian>::cell_verts(in2.as_bytes(), "CELLS", FileType::Ascii);
        assert_eq!(
            f,
            Ok((
                "other".as_bytes(),
                VertexNumbers::Legacy {
                    num_cells: 1,
                    vertices: vec![2, 1, 2]
                }
            ))
        );
    }
    #[test]
    fn cell_type_test() {
        let f = VtkParser::<NativeEndian>::cell_type("2".as_bytes());
        assert_eq!(f, Ok(("".as_bytes(), CellType::PolyVertex)));
        let f = VtkParser::<NativeEndian>::cell_type("10".as_bytes());
        assert_eq!(f, Ok(("".as_bytes(), CellType::Tetra)));
    }

    macro_rules! test {
        ($fn:ident ($in:expr, $($args:expr),*) => ($rem:expr, $out:expr)) => {
            assert_eq!(VtkParser::<NativeEndian>::$fn($in.as_bytes(), $($args),*), Ok(($rem.as_bytes(), $out.clone())));
        };
        ($fn:ident ($in:expr) => ($rem:expr, $out:expr)) => {
            assert_eq!(VtkParser::<NativeEndian>::$fn($in.as_bytes()), Ok(($rem.as_bytes(), $out.clone())));
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
        test!(cell_types(in1, FileType::Ascii) => ("other", out1));
        test!(cell_types(in2, FileType::Ascii) => ("other", out2));
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

        test!(unstructured_grid(in1, FileType::Ascii) => ("other", out1));
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
        test!(attribute(in1, 6, FileType::Ascii) => ("", out1));
    }
    #[test]
    fn attributes_test() {
        // empty cell attributes
        test!(point_or_cell_attributes("\n", "CELL_DATA", FileType::Ascii) => Vec::new());
        // empty point attributes
        test!(point_or_cell_attributes("", "POINT_DATA", FileType::Ascii) => Vec::new());
        // empty
        test!(attributes("\n", FileType::Ascii) => Attributes::new());
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
        test!(point_or_cell_attributes(in1, "CELL_DATA", FileType::Ascii) => out1);
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
        test!(attributes(in2, FileType::Ascii) => out2);
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
        test!(dataset(in1, FileType::Ascii) => out1);
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
        test!(dataset(in1, FileType::Ascii) => out1);
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
        test!(dataset(in1, FileType::Ascii) => out1);
    }
}
