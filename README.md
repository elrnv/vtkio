# vtkio

A parser and writer for the Visualization Toolkit (VTK) [legacy file
format](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf).

This is meant to be a feature complete parser of the legacy VTK file format. However, I have only
used this parser for tetrahedral and triangle meshes, and I am still lacking real world tests. Pull
requests welcome.

This parser is written using [nom](https://github.com/Geal/nom).

# Examples

There are many sample files can be found in the `assets` directory. For the following example, we
will load a VTK file named `tet.vtk`, modify it and write it back in ASCII VTK format.

```rust
extern crate vtkio;
use vtkio::model::*; // import model definition of a VTK file
use vtkio::{import, export_ascii};
fn main() {
    use std::path::PathBuf;
    let file_path = PathBuf::from("tet.vtk");
    
    let mut vtk_file = import(&file_path)
        .expect(&format!("Failed to load file: {:?}", file_path));
    
    vtk_file.version = Version::new((4,2)); // arbitrary change

    export_ascii(vtk_file, &file_path)
        .expect(&format!("Failed to save file: {:?}", file_path));
}
```

# Notes

If you are working with binary files produced by [ParaView](https://www.paraview.org/), it is likely
that they will be written in big endian format regardless of your platform. To load binary files in a
specific endianness, use the `import_le` and `import_be` functions for little and big endian files
respectively.

# License

This work is licensed under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).
