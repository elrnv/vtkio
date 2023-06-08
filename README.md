# vtkio

A parser and writer for the Visualization Toolkit (VTK) [file
formats](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/).

[![On crates.io](https://img.shields.io/crates/v/vtkio.svg)](https://crates.io/crates/vtkio)
[![On docs.rs](https://docs.rs/vtkio/badge.svg)](https://docs.rs/vtkio/)
[![Build Status](https://github.com/elrnv/vtkio/workflows/CI/badge.svg)](https://github.com/elrnv/vtkio/actions)

This is meant to be a feature complete parser of Legacy and XML VTK file formats. Both serial and
parallel XML file formats are supported.

The Legacy format parser is written using [nom](https://crates.io/crates/nom).
XML VTK files are import and exported with [`quick-xml`](https://crates.io/crates/quick-xml) and [`serde`](https://crates.io/crates/serde) crates.


# Usage

To use this library simply add the crate name to your `Cargo.toml` file:

```rust
[dependencies]
vtkio = "0.6"
```


## Examples

Many sample files can be found in the `assets` directory. Below are some examples for using this library. 

### Import/Export

Below we load a VTK file named `tet.vtk`, modify it and write it back in Legacy ASCII format.

```rust
use vtkio::model::*; // import model definition of a VTK file
fn main() {
    use std::path::PathBuf;
    let file_path = PathBuf::from("assets/tet.vtk");

    let mut vtk_file = Vtk::import(&file_path)
        .expect(&format!("Failed to load file: {:?}", file_path));

    vtk_file.version = Version::new((4,2)); // arbitrary change

    vtk_file.export_ascii(&file_path)
        .expect(&format!("Failed to save file: {:?}", file_path));
}
```

The next two examples show how to create new `Vtk` instances manually.


### Simple triangular cell

Here, we create a Vtk instance containing a single triangle represented as a cell of an unstructured grid.

```rust
fn make_triangle() -> Vtk {
    Vtk {
        version: Version { major: 4, minor: 2 },
        title: String::new(),
        byte_order: ByteOrder::BigEndian,
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: IOBuffer::F64(vec![
              // coordinates of node 0
              -1.0, -1.0, 0.0,

               // coordinates of node 1
               1.0, -1.0, 0.0,

               // coordinates of node 2
               1.0,  1.0, 0.0,
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    // connect node 0, 1, 2 (in this order)
                    connectivity: vec![0, 1, 2],

                    // only one group of size 3
                    offsets: vec![3],
                },
                // only one cell of type triangle
                types: vec![CellType::Triangle; 1],
            },
            data: Attributes {
                ..Default::default()
            },
        }),
    }
}
```


### Mixing Cell Types

The following example creates a mesh with a triangle and a quadrilateral.

```rust
fn make_mixed_flat_elements() -> Vtk {
    Vtk {
        version: Version { major: 4, minor: 2 },
        title: String::new(),
        byte_order: ByteOrder::BigEndian,
        file_path: None,
        data: DataSet::inline(UnstructuredGridPiece {
            points: IOBuffer::F64(vec![
                -1.0, -1.0, 0.0,
                 1.0, -1.0, 0.0,
                 1.0,  1.0, 0.0,
                -1.0,  1.0, 0.0,
                 2.0, -1.0, 0.2,
                 2.0,  1.0, 0.2,
            ]),
            cells: Cells {
                cell_verts: VertexNumbers::XML {
                    connectivity: vec![
                      // nodes of triangle
                      0, 1, 2,

                      // nodes of quadrilateral
                      1, 4, 5, 2,
                    ],
                    offsets: vec![
                      // number of nodes cell 1
                      3,

                      // number of nodes cell 1 + number of nodes of cell 2
                      // 3 + 4 = 7
                      7
                    ],
                },
                types: vec![
                  CellType::Triangle,
                  CellType::Quad
                ],
            },
            data: Attributes {
                ..Default::default()
            },
        }),
    }
}
```


## Features

There are two main features available:

- XML File support via the `xml` feature flag (enabled by default).
  This allows importing and exporting VTK files in the modern XML format. If disabled, only the legacy
  file format is supported, however the build is faster since it does not include additional
  dependencies (`serde` and `quick-xml`) and code needed to parse and write XML files.
- Compression via the `compression` feature flag (enabled by default).
  This flag exposes additional APIs to export and import compressed VTK files (only for XML format).
  This feature has no benefit when the `xml` feature is disabled.

To disable the features above simply set `default-features` to `false`. To enable a specific feature
add it to the list under `features`. For instance to disable only the `compression` feature, add the
`vtkio` dependency as

```rust
[dependencies]
vtkio = { version = "0.6", default-features = false, features = ["xml"] }
```

To disable all additional features use

```rust
[dependencies]
vtkio = { version = "0.6", default-features = false }
```

# Changes

Version 0.3 of the crate supports only Legacy VTK formats. For a list of changes
introduced in the new versions of `vtkio` (v0.4+) see the [CHANGELOG](CHANGELOG.md).

# License

This repository is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
 * MIT License ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.

Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in
the Work by You, as defined in the Apache-2.0 license, shall be dual licensed as above, without
any additional terms or conditions.
