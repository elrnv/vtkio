
# Release 0.4

This release most notably adds support for importing and exporting VTK files in the modern XML
format.

The XML support is provided via an additional `xml::VTKFile` type, which stores xml specific
information which can be directly serialized and deserialized with `serde` and `quick-xml` into the
corresponding VTK XML file type.

This means that an additional pass is required to convert the XML type into the underlying
`model::Vtk` type used for vtkio, which unifies legacy and xml I/O and facilitates the lazy loading
of parallel XML formats. Performance sensitive applications may chose to work with the `xml::VTKFile`
type directly, however the `model::Vtk` API is much more comprehensive and easier to work with.
Notably, `xml::VTKFile` stores loaded data buffers as unencoded and potentially compressed strings
and byte arrays, whereas `model::Vtk` stores the decoded uncompressed data ready for processing.

In order to facilitate import and export for both legacy and xml formats, the underlying VTK data
model (`model::Vtk`) was updated. The following outlines most of the critical changes that were made
in version 0.4, particularly in the `model` module:

- `IOBuffer` was simplified into a simple enum of `Vec`s of different predetermined scalar types.
  This means that a number of member functions are now unavailable (and many are unnecessary) on
  `IOBuffer`s.

- a `byte_order` field was added to the `Vtk` model to facilitate automatic determination byte order
  on import and export. This means that `import` and `export` functions will automatically encode
  data in the byte order provided in that field. Byte order can still be overridden
  with `export_{le,be}` or by changing the `byte_order` field in the `Vtk` file directly.

- `Attribute`s have been completely reformulated to distinguish between Field attributes which are
  available only in Legacy VTK formats and the rest.
  The distinction between the "kind" of attribute (ColorScalars, Scalars, Vectors, etc.) is
  offloaded to a separate enum called `ElementType`. Because XML files don't necessarily need a
  "kind" for each attribute, `ElementType` can take on a `Generic` variant, which can represent any
  number of components per element.

- `DataArray`s as well as `Attribute`s are equipped with convenience constructors for creating
  arrays with a specific type.

- `call_numeric_buffer_fn` macro has been removed. A new macro that can tersely abstract over the
  type of the underlying vector is `match_buf`, which evaluates an expression with a binding to the
  underlying vector. As before this is useful for expressions whose return type is the same
  regardless of the type of the vector, otherwise the caller must match on the `IOBuffer` variants
  explicitly.

  For example, suppose we had a function

  ```rust
  fn display<T: std::fmt::Display>(buf: &IOBuffer) { ... }
  ```

  that printed the buffer in some way. Previously we would need to call this function as follows

  ```rust
  call_numeric_buffer_fn!( display<_>(&buf) or {} );
  ```

  With the current changes, we would need to rewrite the `display` function in terms of a `Vec`
  (or slice) instead as follows:
  ```rust
  fn display<T: std::fmt::Display>(slice: &[T]) { ... }
  ```

  and call it with `match_buf` like so:

  ```rust
  match_buf!(&buf, v => display(v.as_slice()));
  ```
  which translates to
  
  ```rust
  match &buf {
      IOBuffer::Bit(v) => display(v.as_slice()),
      IOBuffer::U8(v) => display(v.as_slice()),
      IOBuffer::I8(v) => display(v.as_slice()),
      IOBuffer::U16(v) => display(v.as_slice()),
      IOBuffer::I16(v) => display(v.as_slice()),
      IOBuffer::U32(v) => display(v.as_slice()),
      IOBuffer::I32(v) => display(v.as_slice()),
      IOBuffer::U64(v) => display(v.as_slice()),
      IOBuffer::I64(v) => display(v.as_slice()),
      IOBuffer::F32(v) => display(v.as_slice()),
      IOBuffer::F64(v) => display(v.as_slice()),
  }
  ```

- `DataSet` has been decomposed into `Piece`s to be compatible with the XML vtk format.
  Each `DataSet` can now contain multiple pieces. Each piece can be stored either inline alongside the
  data set (as before) or they can be loaded lazily from other referenced vtk files as described in
  the parallel XML vtk format. Since each loaded piece is itself a DataSet, there are in total 3
  variants of a `Piece`: `Inline`, `Source` and `Loaded`. `Inline` pieces contain the actual piece
  data under the `PieceData` type. See the documentation for details.

- An optional `MetaData` type is attached to a data set to store meta information about the pieces
  referenced within. This is only useful when the pieces are not loaded eagerly with the data set.
  The parallel XML formats provide the additional meta data, which allows users to set up the data
  structures without having to load the actual data.

- `DataType` was renamed to `ScalarType` to be more consistent with modern VTK formats. The names of
  the variants were also renamed to resemble `Rust` numeric types rather than C/C++.

- The `StructuredPoints` data set variant was renamed to `ImageData` to reflect modern vtk formats.

- A new `Extent` type was introduced to abstract between Legacy and XML formats. This type describes
  the extent of a grid-like structure (i.e. `ImageData`, `StructuredGrid` and `RectilinearGrid`
  data set types) or piece. See the documentation for details.

- The coordinates of the `RectilinearGrid` type were split into a separate struct containing the
  individual `x`, `y` and `z` coordinate data arrays.

- Data arrays in the `Vtk` data structure can be stored in 3 different ways, two of which
  specialize the generic `DataArrayBase` struct and one is a plain old `IOBuffer`.
  Most data arrays can now be named, with the exception of `Coordinates`, `Points` and `Cells`
  arrays, since those are always unique.

