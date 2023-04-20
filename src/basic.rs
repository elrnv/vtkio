/// This enum indicates if bulk data is saved in binary.
/// NOTE: VTK files are saved in ASCII format with bulk data optionally saved in
/// Binary among ASCII type keywords.  Binary data must be placed into the file
/// immediately after the "newline" (`\n`) character from the previous ASCII
/// keyword and parameter sequence. For example point positions and cell indices
/// and types can be saved in Binary in VTK files.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum FileType {
    Binary,
    ASCII,
}

pub(crate) mod parsers {
    use nom::character::complete::line_ending;
    use nom::character::streaming::space0;
    use nom::error::ParseError;
    use nom::sequence::delimited;
    use nom::sequence::terminated;
    use nom::IResult;

    /// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
    /// trailing space characters (" " & "\n"), returning the output of `inner`.
    pub fn sp<'a, F, O, E: ParseError<&'a [u8]>>(
        inner: F,
    ) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
    where
        F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>,
    {
        delimited(space0, inner, space0)
    }

    /// A combinator that takes a parser `inner` and produces a parser that also consumes trailing
    /// line endings ("\r\n" & "\n"), returning the output of `inner`.
    pub fn line<'a, F, O, E: ParseError<&'a [u8]>>(
        inner: F,
    ) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
    where
        F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>,
    {
        terminated(inner, line_ending)
    }
}

#[cfg(test)]
mod tests {
    /*
    use super::*;
    use byteorder::BigEndian;
    use nom::IResult;

    #[test]
    fn can_parse_float() {
        assert_eq!(real::<f32>(&b"-0.00005"[..]).unwrap().1, -0.00005);
        assert_eq!(real::<f32>(&b"4."[..]).unwrap().1, 4.0);
        assert_eq!(real::<f32>(&b"3"[..]).unwrap().1, 3.0);
        assert_eq!(real::<f32>(&b"-.3"[..]).unwrap().1, -0.3);
        assert_eq!(real::<f32>(&b"3e3"[..]).unwrap().1, 3000.0);
        assert_eq!(real::<f32>(&b"-3.2e2"[..]).unwrap().1, -320.0);
    }
    #[test]
    fn can_parse_int() {
        assert_eq!(integer::<i32>(&b"-1"[..]).unwrap().1, -1);
        assert_eq!(integer::<i32>(&b"1"[..]).unwrap().1, 1);
        assert_eq!(integer::<i32>(&b"43242"[..]).unwrap().1, 43242);
        assert_eq!(integer::<u8>(&b"255"[..]).unwrap().1, 255);
    }
    #[test]
    fn can_parse_binary_float() {
        assert_eq!(
            f32::from_binary::<BigEndian>(&[0u8, 0, 0, 0]).unwrap().1,
            0.0_f32
        );
        assert_eq!(
            f32::from_binary::<BigEndian>(&[62u8, 32, 0, 0]).unwrap().1,
            0.15625_f32
        );
    }
    #[test]
    fn data_test() {
        let f = parse_data_buffer::<f32, BigEndian>("".as_bytes(), 0, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(Vec::<f32>::new()))
        );
        let f = parse_data_buffer::<f32, BigEndian>("3".as_bytes(), 1, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(vec![3.0f32]))
        );
        let f = parse_data_buffer::<f32, BigEndian>("3 32".as_bytes(), 2, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(vec![3.0f32, 32.0]))
        );
        let f = parse_data_buffer::<f32, BigEndian>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "".as_bytes(),
                IOBuffer::from(vec![3.0f32, 32.0, 32.0, 4.0e3])
            )
        );
        let f = parse_data_buffer::<f64, BigEndian>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "".as_bytes(),
                IOBuffer::from(vec![3.0f64, 32.0, 32.0, 4.0e3])
            )
        );
    }
    */
}
