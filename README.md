# vtkio

A parser and writer for the Visualization Toolkit (VTK) [file
formats](https://lorensen.github.io/VTKExamples/site/VTKFileFormats/).

[![On crates.io](https://img.shields.io/crates/v/vtkio.svg)](https://crates.io/crates/vtkio)
[![On docs.rs](https://docs.rs/vtkio/badge.svg)](https://docs.rs/vtkio/)
[![Build status](https://travis-ci.org/elrnv/vtkio.svg?branch=master)](https://travis-ci.org/elrnv/vtkio)

This is meant to be a feature complete parser of Legacy and XML VTK file formats. Both serial and
parallel XML file formats are supported.

The Legacy format parser is written using [nom](https://crates.io/crates/nom).
XML VTK files are import and exported with [`quick-xml`](https://crates.io/crates/quick-xml) and [`serde`](https://crates.io/crates/serde) crates.

# Examples

Many sample files can be found in the `assets` directory. For the following example, we
will load a VTK file named `tet.vtk`, modify it and write it back in Legacy ASCII format.

```rust
use vtkio::model::*; // import model definition of a VTK file
use vtkio::{import, export_ascii};
fn main() {
    use std::path::PathBuf;
    let file_path = PathBuf::from("assets/tet.vtk");
    
    let mut vtk_file = import(&file_path)
        .expect(&format!("Failed to load file: {:?}", file_path));
    
    vtk_file.version = Version::new((4,2)); // arbitrary change

    export_ascii(vtk_file, &file_path)
        .expect(&format!("Failed to save file: {:?}", file_path));
}
```

# Changes

Version 0.3 of the crate supports only Legacy VTK formats. For a list of changes
introduced in the new versions of `vtkio` (v0.4+) see the [CHANGELOG](CHANGELOG.md).

# License

This repository is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
 * MIT License ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.
