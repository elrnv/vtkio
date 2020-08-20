use vtkio::Error;

type Result = std::result::Result<(), Error>;

#[test]
fn example() -> Result {
    let xml = "
    <?xml version=\"1.0\"?>
    <VTKFile type=\"PPolyData\" version=\"0.1\" byte_order=\"LittleEndian\">
        <PPolyData GhostLevel=\"0\">
            <PPointData Scalars=\"my_scalars\">
            <PDataArray type=\"Float32\" Name=\"my_scalars\"/>
            </PPointData>
            <PCellData Scalars=\"cell_scalars\" Normals=\"cell_normals\">
            <PDataArray type=\"Int32\" Name=\"cell_scalars\"/>
            <PDataArray type=\"Float32\" Name=\"cell_normals\" NumberOfComponents=\"3\"/>
            </PCellData>
            <PPoints>
            <PDataArray type=\"Float32\" NumberOfComponents=\"3\"/>
            </PPoints>
            <Piece Source=\"polyEx0.vtp\"/>
        </PPolyData>
    </VTKFile>";

    let xmlp = "<?xml version=\"1.0\"?>
    <VTKFile type=\"PolyData\" version=\"0.1\" byte_order=\"LittleEndian\">
        <PolyData>
            <Piece NumberOfPoints=\"8\" NumberOfVerts=\"0\" NumberOfLines=\"0\"NumberOfStrips=\"0\" NumberOfPolys=\"6\">
            <Points>
            <DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">000100110010001101111011</DataArray>
            </Points>
            <PointData Scalars=\"my_scalars\">
            <DataArray type=\"Float32\" Name=\"my_scalars\" format=\"ascii\">01234567</DataArray>
            </PointData>
            <CellData Scalars=\"cell_scalars\" Normals=\"cell_normals\">
            <DataArray type=\"Int32\" Name=\"cell_scalars\" format=\"ascii\">012345</DataArray>
            <DataArray type=\"Float32\" Name=\"cell_normals\"NumberOfComponents=\"3\" format=\"ascii\">00-10010-10010-100100</DataArray>
            </CellData>
            <Polys>
            <DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">012345670154237604731265</DataArray>
            <DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">4812162024</DataArray>
            </Polys>
            </Piece>
        </PolyData>
    </VTKFile>
    ";
    Ok(())
}
