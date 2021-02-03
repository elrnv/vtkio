// TODO: migrating to a newer version of nom could reduce the detected "cognitive complexity"
// caused by macros.
#![allow(clippy::cognitive_complexity)]

use std::marker::PhantomData;
use std::str;

use byteorder::{BigEndian, ByteOrder, LittleEndian, NativeEndian};
use nom::{self, eol, ErrorKind, IResult};
use num_traits::FromPrimitive;

pub use crate::basic::*;
use crate::model::*;

use crate::model::ByteOrder as ByteOrderTag;

/*
 * Parsing routines
 */

// Parse the file version
named!(version<&[u8], Version>, sp!(
 do_parse!(
     take_until!("#") >>
     tag!("#") >>
     tag_no_case!("vtk") >>
     tag_no_case!("DataFile") >>
     tag_no_case!("Version") >>
     ver: separated_pair!(u8_b, tag!("."), u8_b) >>
     eol >>
     (Version::new(ver))
     )
 )
);

named!(file_type<&[u8], FileType>,
       alt!( tag_no_case!("ASCII") => { |_| FileType::ASCII } |
             tag_no_case!("BINARY") => { |_| FileType::Binary } ) );

named!(title<&[u8], &str>, map_res!(
  do_parse!(
      ttl: take_until!("\n") >>
      eol >>
      (ttl)),
      str::from_utf8 )
);

named!(header<&[u8], (Version, String, FileType)>, sp!(
     do_parse!(
         ver: version >>
         ttl: title >>
         ft:  file_type >>
         ((ver, String::from(ttl), ft))
         )
    )
);

named!(data_type< &[u8], ScalarType >, alt!(
        tag_no_case!("bit")            => { |_| ScalarType::Bit } |
        tag_no_case!("int")            => { |_| ScalarType::I32 } |
        tag_no_case!("char")           => { |_| ScalarType::I8 } |
        tag_no_case!("long")           => { |_| ScalarType::I64 } |
        tag_no_case!("short")          => { |_| ScalarType::I16 } |
        tag_no_case!("float")          => { |_| ScalarType::F32 } |
        tag_no_case!("double")         => { |_| ScalarType::F64 } |
        tag_no_case!("unsigned_int")   => { |_| ScalarType::U32 } |
        tag_no_case!("unsigned_char")  => { |_| ScalarType::U8 } |
        tag_no_case!("unsigned_long")  => { |_| ScalarType::U64 } |
        tag_no_case!("unsigned_short") => { |_| ScalarType::U16 } ));

named!(pub usize_b<&[u8], usize>, call!(integer) );
named!(pub u32_b<&[u8], u32>, call!(integer) );
named!(pub u8_b<&[u8], u8>, call!(integer) );
named!(pub f32_b<&[u8], f32>, call!(real::<f32>) );

named!(name, take_till!(|x: u8| b" \t\n\r".contains(&x)));

enum Axis {
    X,
    Y,
    Z,
}

/**
 * Mesh data parsing.
 */
pub struct VtkParser<BO: ByteOrder>(PhantomData<BO>);

/// Helper struct for parsing polygon topology sections.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum PolyDataTopology {
    Verts = 0,
    Lines,
    Polys,
    Strips,
}

impl<BO: ByteOrder> VtkParser<BO> {
    #[allow(unused_variables)]
    fn points(input: &[u8], ft: FileType) -> IResult<&[u8], IOBuffer> {
        do_parse!(
            input,
            n: ws!(do_parse!(tag_no_case!("POINTS") >> n: u32_b >> (n)))
                >> vec: switch!(
                       do_parse!(
                           dt: sp!( data_type ) >>
                           tag!("\n") >>
                           (dt) ),
                                ScalarType::F32 => call!( parse_data_buffer::<f32, BO>, 3*n as usize, ft ) |
                                ScalarType::F64 => call!( parse_data_buffer::<f64, BO>, 3*n as usize, ft ) )
                >> (vec)
        )
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
        do_parse!(
            input,
            n: ws!(do_parse!(tag_no_case!(tag) >> n: u32_b >> (n)))
                >> size: sp!(u32_b)
                >> tag!("\n")
                >> data: call!(parse_data_vec::<u32, BO>, size as usize, ft)
                >> ({
                    VertexNumbers::Legacy {
                        num_cells: n,
                        vertices: data,
                    }
                })
        )
    }

    #[allow(unused_variables)]
    fn coordinates(input: &[u8], axis: Axis, ft: FileType) -> IResult<&[u8], IOBuffer> {
        let tag = match axis {
            Axis::X => "X_COORDINATES",
            Axis::Y => "Y_COORDINATES",
            Axis::Z => "Z_COORDINATES",
        };
        do_parse!(
            input,
            n: ws!(do_parse!(tag_no_case!(tag) >> n: u32_b >> (n)))
                >> vec: switch!(
                       do_parse!(
                           dt: sp!( data_type ) >>
                           tag!("\n") >>
                           (dt) ),
                                ScalarType::F32 => call!( parse_data_buffer::<f32, BO>, n as usize, ft ) |
                                ScalarType::F64 => call!( parse_data_buffer::<f64, BO>, n as usize, ft ) )
                >> (vec)
        )
    }

    /// Recognize and throw away `METADATA` block. Metadata is spearated by an empty line.
    fn meta(input: &[u8]) -> IResult<&[u8], ()> {
        complete!(
            input,
            ws!(do_parse!(
                tag_no_case!("METADATA") >> take_until!("\n\n") >> ()
            ))
        )
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

    named!(
        lookup_table,
        alt_complete!(
        sp!( do_parse!( tag_no_case!("LOOKUP_TABLE") >> n: name >> (n) ) ) |
        eof!() => { |_| &b""[..] } |
        eol
        )
    );

    fn attribute_scalars(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("SCALARS")
                    >> name: map_res!(name, str::from_utf8)
                    >> dt: data_type
                    >> num_comp: opt!(u32_b)
                    >> lookup_tbl_name: opt!(map_res!(Self::lookup_table, str::from_utf8))
                    >> data: call!(
                        Self::attribute_data,
                        num_comp.unwrap_or(1) as usize * num_elements,
                        dt,
                        ft
                    )
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::Scalars {
                            num_comp: num_comp.unwrap_or(1),
                            lookup_table: lookup_tbl_name.and_then(|x| if x == "default" {
                                None
                            } else {
                                Some(String::from(x))
                            }),
                        },
                        data
                    }))
            )
        )
    }

    fn attribute_lookup_table(input: &[u8], ft: FileType) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("LOOKUP_TABLE")
                    >> name: map_res!(name, str::from_utf8)
                    >> num_elements: u32_b
                    >> data: call!(
                        Self::attribute_data,
                        4 * num_elements as usize,
                        ScalarType::F32,
                        ft
                    )
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::LookupTable,
                        data
                    }))
            )
        )
    }

    /// Helper to `attribute_color_scalars`. This function calls the appropriate data parser for color
    /// scalars.
    fn attribute_color_scalars_data(
        input: &[u8],
        n: usize,
        ft: FileType,
    ) -> IResult<&[u8], IOBuffer> {
        match ft {
            FileType::ASCII => Self::attribute_data(input, n, ScalarType::F32, ft),
            FileType::Binary => Self::attribute_data(input, n, ScalarType::U8, ft),
        }
    }

    fn attribute_color_scalars(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("COLOR_SCALARS")
                    >> name: map_res!(name, str::from_utf8)
                    >> num_comp: u32_b
                    >> data: call!(
                        Self::attribute_color_scalars_data,
                        num_comp as usize * num_elements,
                        ft
                    )
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::ColorScalars(num_comp),
                        data
                    }))
            )
        )
    }

    fn attribute_vectors(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("VECTORS")
                    >> name: map_res!(name, str::from_utf8)
                    >> dt: data_type
                    >> data: call!(Self::attribute_data, 3 * num_elements, dt, ft)
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::Vectors,
                        data
                    }))
            )
        )
    }

    fn attribute_normals(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("NORMALS")
                    >> name: map_res!(name, str::from_utf8)
                    >> dt: data_type
                    >> data: call!(Self::attribute_data, 3 * num_elements, dt, ft)
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::Normals,
                        data
                    }))
            )
        )
    }

    fn attribute_tex_coords(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("TEXTURE_COORDINATES")
                    >> name: map_res!(name, str::from_utf8)
                    >> dim: u32_b
                    >> dt: data_type
                    >> data: call!(Self::attribute_data, dim as usize * num_elements, dt, ft)
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::TCoords(dim),
                        data
                    }))
            )
        )
    }

    fn attribute_tensors(
        input: &[u8],
        num_elements: usize,
        ft: FileType,
    ) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("TENSORS")
                    >> name: map_res!(name, str::from_utf8)
                    >> dt: data_type
                    >> data: call!(Self::attribute_data, 9 * num_elements, dt, ft)
                    >> opt!(Self::meta)
                    >> (Attribute::DataArray(DataArray {
                        name: String::from(name),
                        elem: ElementType::Tensors,
                        data
                    }))
            )
        )
    }

    fn attribute_field_array(input: &[u8], ft: FileType) -> IResult<&[u8], FieldArray> {
        ws!(
            input,
            do_parse!(
                name: map_res!(name, str::from_utf8)
                    >> num_comp: u32_b
                    >> num_tuples: u32_b
                    >> dt: data_type
                    >> data: call!(
                        Self::attribute_data,
                        (num_comp * num_tuples) as usize,
                        dt,
                        ft
                    )
                    >> opt!(Self::meta)
                    >> (FieldArray {
                        name: String::from(name),
                        elem: num_comp,
                        data
                    })
            )
        )
    }

    fn attribute_field(input: &[u8], ft: FileType) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("FIELD")
                    >> name: map_res!(name, str::from_utf8)
                    >> n: u32_b
                    >> data_array:
                        many_m_n!(
                            n as usize,
                            n as usize,
                            call!(Self::attribute_field_array, ft)
                        )
                    >> (Attribute::Field {
                        name: String::from(name),
                        data_array
                    })
            )
        )
    }

    fn attribute(input: &[u8], num_elements: usize, ft: FileType) -> IResult<&[u8], Attribute> {
        ws!(
            input,
            alt!(
                call!(Self::attribute_scalars, num_elements, ft)
                    | call!(Self::attribute_color_scalars, num_elements, ft)
                    | call!(Self::attribute_lookup_table, ft)
                    | call!(Self::attribute_vectors, num_elements, ft)
                    | call!(Self::attribute_normals, num_elements, ft)
                    | call!(Self::attribute_tex_coords, num_elements, ft)
                    | call!(Self::attribute_tensors, num_elements, ft)
                    | call!(Self::attribute_field, ft)
            )
        )
    }

    fn point_attributes(input: &[u8], ft: FileType) -> IResult<&[u8], Vec<Attribute>> {
        ws!(
            input,
            alt_complete!(
            do_parse!(
                tag_no_case!("POINT_DATA") >>
                n: sp!(u32_b) >>
                vec: many0!( call!( Self::attribute, n as usize, ft ) ) >>
                (vec)
                ) |
            ws!( eof!() ) => { |_| Vec::new() }
            )
        )
    }

    fn cell_attributes(input: &[u8], ft: FileType) -> IResult<&[u8], Vec<Attribute>> {
        ws!(
            input,
            alt_complete!(
                do_parse!(
                    ws!( tag_no_case!("CELL_DATA") ) >>
                    n: sp!( u32_b ) >>
                    vec: many0!( call!( Self::attribute, n as usize, ft ) ) >>
                    (vec)
                ) |
                ws!( eof!() ) => { |_| Vec::new() }
            )
        )
    }

    fn attributes(input: &[u8], ft: FileType) -> IResult<&[u8], Attributes> {
        ws!(
            input,
            do_parse!(
                c1: opt!(call!(Self::cell_attributes, ft))
                    >> p: opt!(call!(Self::point_attributes, ft))
                    >> c2: opt!(call!(Self::cell_attributes, ft))
                    >> (Attributes {
                        point: p.unwrap_or_default(),
                        cell: if let Some(c) = c1 {
                            c
                        } else {
                            c2.unwrap_or_default()
                        }
                    })
            )
        )
    }

    /// Parse structured points dataset.
    fn structured_points(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("STRUCTURED_POINTS")
                    >> parms:
                        permutation!(
                            do_parse!(
                                tag_no_case!("DIMENSIONS")
                                    >> nx: u32_b
                                    >> ny: u32_b
                                    >> nz: u32_b
                                    >> ([nx, ny, nz])
                            ),
                            do_parse!(
                                tag_no_case!("ORIGIN")
                                    >> x: f32_b
                                    >> y: f32_b
                                    >> z: f32_b
                                    >> ([x, y, z])
                            ),
                            do_parse!(
                                alt_complete!(
                                    tag_no_case!("SPACING") | tag_no_case!("ASPECT_RATIO")
                                ) >> sx: f32_b
                                    >> sy: f32_b
                                    >> sz: f32_b
                                    >> ([sx, sy, sz])
                            )
                        )
                    >> data: call!(Self::attributes, ft)
                    >> (DataSet::ImageData {
                        extent: Extent::Dims(parms.0),
                        origin: parms.1,
                        spacing: parms.2,
                        meta: None,
                        pieces: vec![Piece::Inline(Box::new(ImageDataPiece {
                            extent: Extent::Dims(parms.0),
                            data
                        }))]
                    })
            )
        )
    }

    /// Parse structured grid dataset.
    fn structured_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("STRUCTURED_GRID")
                    >> dims: do_parse!(
                        tag_no_case!("DIMENSIONS")
                            >> nx: u32_b
                            >> ny: u32_b
                            >> nz: u32_b
                            >> ([nx, ny, nz])
                    )
                    >> points: call!(Self::points, ft)
                    >> opt!(Self::meta)
                    >> data: call!(Self::attributes, ft)
                    >> (DataSet::inline(StructuredGridPiece {
                        extent: Extent::Dims(dims),
                        points,
                        data
                    }))
            )
        )
    }

    /// Parse rectilinear grid dataset.
    fn rectilinear_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("RECTILINEAR_GRID")
                    >> dims: do_parse!(
                        tag_no_case!("DIMENSIONS")
                            >> nx: u32_b
                            >> ny: u32_b
                            >> nz: u32_b
                            >> ([nx, ny, nz])
                    )
                    >> x: call!(Self::coordinates, Axis::X, ft)
                    >> y: call!(Self::coordinates, Axis::Y, ft)
                    >> z: call!(Self::coordinates, Axis::Z, ft)
                    >> data: call!(Self::attributes, ft)
                    >> opt!(complete!(Self::meta))
                    >> (DataSet::inline(RectilinearGridPiece {
                        extent: Extent::Dims(dims),
                        coords: Coordinates { x, y, z },
                        data
                    }))
            )
        )
    }

    /// Parse field dataset.
    fn field_data(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        let res = Self::attribute_field(input, ft);
        match res {
            IResult::Done(i, o) => {
                if let Attribute::Field { name, data_array } = o {
                    IResult::Done(i, DataSet::Field { name, data_array })
                } else {
                    IResult::Error(nom::Err::Code(ErrorKind::Custom(1u32)))
                }
            }
            IResult::Incomplete(e) => IResult::Incomplete(e),
            IResult::Error(e) => IResult::Error(e),
        }
    }

    // Parse a single cell type. Essentially a byte converted to` CellType` enum.
    named!(pub cell_type<&[u8], CellType>,
    map_opt!( u8_b, |x| CellType::from_u8(x) )
    );

    named!(pub cell_type_binary<&[u8], CellType>,
    map_opt!( i32::from_binary::<BO>, |x| CellType::from_u8(x as u8) )
    );

    fn cell_type_data(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<CellType>> {
        match ft {
            FileType::ASCII => many_m_n!(input, n, n, ws!(Self::cell_type)),
            FileType::Binary => many_m_n!(input, n, n, Self::cell_type_binary),
        }
    }

    /// Parse cell types for unstructured grids
    fn cell_types(input: &[u8], ft: FileType) -> IResult<&[u8], Vec<CellType>> {
        do_parse!(
            input,
            ws!(tag_no_case!("CELL_TYPES"))
                >> n: sp!(usize_b)
                >> tag!("\n")
                >> data: dbg!(call!(Self::cell_type_data, n, ft))
                >> (data)
        )
    }

    /// Parse UNSTRUCTURED_GRID type dataset
    fn unstructured_grid(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        ws!(
            input,
            do_parse!(
                tag_no_case!("UNSTRUCTURED_GRID")
                    >> p: call!(Self::points, ft)
                    >> opt!(Self::meta)
                    >> cell_verts: call!(Self::cell_verts, "CELLS", ft)
                    >> types: call!(Self::cell_types, ft)
                    >> data: call!(Self::attributes, ft)
                    >> (DataSet::inline(UnstructuredGridPiece {
                        points: p,
                        cells: Cells { cell_verts, types },
                        data
                    }))
            )
        )
    }

    /// Parse PolyData topology
    fn poly_data_topo(
        input: &[u8],
        ft: FileType,
    ) -> IResult<&[u8], (PolyDataTopology, VertexNumbers)> {
        alt_complete!(
            input,
            map!(call!(Self::cell_verts, "LINES", ft), |x| {
                (PolyDataTopology::Lines, x)
            }) | map!(call!(Self::cell_verts, "POLYGONS", ft), |x| {
                (PolyDataTopology::Polys, x)
            }) | map!(call!(Self::cell_verts, "VERTICES", ft), |x| {
                (PolyDataTopology::Verts, x)
            }) | map!(call!(Self::cell_verts, "TRIANGLE_STRIPS", ft), |x| {
                (PolyDataTopology::Strips, x)
            })
        )
    }

    /// Parse POLYDATA type dataset
    #[allow(unused_comparisons)] // Suppress the warning of using 0 in many_m_n!(..)
    fn poly_data(input: &[u8], ft: FileType) -> IResult<&[u8], DataSet> {
        do_parse!(
            input,
            tag_no_case!("POLYDATA")
                >> points: call!(Self::points, ft)
                >> opt!(Self::meta)
                >> topo1: opt!(call!(Self::poly_data_topo, ft))
                >> topo2: opt!(call!(Self::poly_data_topo, ft))
                >> topo3: opt!(call!(Self::poly_data_topo, ft))
                >> topo4: opt!(call!(Self::poly_data_topo, ft))
                >> data: call!(Self::attributes, ft)
                >> ({
                    // The following algorithm is just to avoid unnecessary cloning.
                    // There may be a simpler way to do this.
                    let mut topos = [topo1, topo2, topo3, topo4];
                    let vertsi = topos
                        .iter()
                        .position(|x| x.as_ref().map(|x| x.0) == Some(PolyDataTopology::Verts));
                    let linesi = topos
                        .iter()
                        .position(|x| x.as_ref().map(|x| x.0) == Some(PolyDataTopology::Lines));
                    let polysi = topos
                        .iter()
                        .position(|x| x.as_ref().map(|x| x.0) == Some(PolyDataTopology::Polys));
                    let stripsi = topos
                        .iter()
                        .position(|x| x.as_ref().map(|x| x.0) == Some(PolyDataTopology::Strips));
                    let mut indices = [0, 1, 2, 3];

                    vertsi.map(|i| {
                        indices.swap(i, 0);
                        topos.swap(i, 0)
                    });
                    linesi.map(|i| {
                        let i = indices[i];
                        indices.swap(i, 1);
                        topos.swap(i, 1)
                    });
                    polysi.map(|i| {
                        let i = indices[i];
                        indices.swap(i, 2);
                        topos.swap(i, 2)
                    });
                    stripsi.map(|i| {
                        let i = indices[i];
                        indices.swap(i, 3);
                        topos.swap(i, 3)
                    });

                    let [verts, lines, polys, strips] = topos;

                    DataSet::inline(PolyDataPiece {
                        points,
                        verts: verts.map(|x| x.1),
                        lines: lines.map(|x| x.1),
                        polys: polys.map(|x| x.1),
                        strips: strips.map(|x| x.1),
                        data,
                    })
                })
        )
    }

    fn dataset(input: &[u8], file_type: FileType) -> IResult<&[u8], DataSet> {
        alt_complete!(
            input,
            do_parse!(
                tag_no_case!("DATASET")
                    >> whitespace
                    >> tn: alt!(
                        call!(Self::poly_data, file_type)
                            | call!(Self::structured_grid, file_type)
                            | call!(Self::rectilinear_grid, file_type)
                            | call!(Self::structured_points, file_type)
                            | call!(Self::unstructured_grid, file_type)
                    )
                    >> (tn)
            ) | call!(Self::field_data, file_type)
        )
    }

    /// Parse the entire vtk file
    fn vtk(input: &[u8]) -> IResult<&[u8], Vtk> {
        complete!(
            input,
            ws!(do_parse!(
                h: header
                    >> d: call!(Self::dataset, h.2)
                    >> (Vtk {
                        version: h.0,
                        // This is ignored in Legacy formats
                        byte_order: ByteOrderTag::BigEndian,
                        title: h.1,
                        data: d,
                        file_path: None,
                    })
            ))
        )
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
}
