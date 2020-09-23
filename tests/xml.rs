use vtkio::Error;

type Result = std::result::Result<(), Error>;

#[test]
fn example() -> Result {
    Ok(())
}
