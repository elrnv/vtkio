use std::io::{BufReader, Read};
use vtkio::{parse_xml, Error};

type Result = std::result::Result<(), Error>;

#[test]
fn box_inline_base64() -> Result {
    let input = include_str!("../assets/box.vtu").as_bytes();
    let vtk = parse_xml(BufReader::new(input));
    std::dbg!(vtk);
    Ok(())
}
