use crate::model::*;

/// Creates a single vertex
pub fn example_vertex() -> Vtk {
    Vtk {
        version: Version { major: 4, minor: 2 },
        title: String::new(),
        byte_order: ByteOrder::BigEndian,
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: IOBuffer::F64(vec![1.0, 1.0, 0.0]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![0],
                    offsets: vec![1],
                },
                types: vec![CellType::Vertex; 1],
            },
            data: Attributes {
                ..Default::default()
            },
        }),
    }
}
