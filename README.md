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

# Usage

To use this library simply add the crate name to your `Cargo.toml` file:

```rust
[dependencies]
vtkio = "0.5"
```

## Examples

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
