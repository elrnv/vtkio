use std::any::Any;
use std::num::NonZeroUsize;
use std::str::FromStr;

use byteorder::ByteOrder;
use num_traits::Zero;

use nom::branch::alt;
use nom::bytes::streaming::{tag, tag_no_case};
use nom::character::complete::{digit1, line_ending, multispace0, space0};
use nom::combinator::{complete, cut, eof, map, map_opt, map_res, opt, recognize};
use nom::error::ParseError;
use nom::multi::many_m_n;
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{IResult, Needed, ParseTo, Parser};

use crate::model::IOBuffer;

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

/*
 * General parser combinator building blocks
 */

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing spaces, tabs, LFs, CRLFs, returning the output of `inner`.
pub fn ws<'a, F, O, E: ParseError<&'a [u8]>>(
    inner: F,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>,
{
    delimited(multispace0, inner, multispace0)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing space and tab characters (" " & "\t"), returning the output of `inner`.
pub fn sp<'a, F, O, E: ParseError<&'a [u8]>>(
    inner: F,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>,
{
    delimited(space0, inner, space0)
}

/// Recognizes a line ending or an eof.
pub fn eol_or_eof<'a, E: ParseError<&'a [u8]>>(input: &'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    alt((line_ending, eof))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes trailing
/// line endings ("\r\n" & "\n"), returning the output of `inner`.
pub fn line<'a, F, O, E: ParseError<&'a [u8]>>(
    inner: F,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>,
{
    terminated(inner, eol_or_eof)
}

/// Parses a `tag_no_case` possibly surrounded by spaces (" " & "\t") and followed by a EOL.
pub fn tag_no_case_line<'a, E: ParseError<&'a [u8]>>(
    tag: &'static str,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], &'a [u8], E> {
    line(sp(tag_no_case(tag)))
}

/// Parses a "block" that is preceded by possibly multiple multispaces and a specific `block_tag`
/// identified by the given parser, returns the result of applying `body` after the `block_tag`.
pub fn tagged_block<'a, O1, O2, E: ParseError<&'a [u8]>, F, G>(
    block_tag: F,
    body: G,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O2, E>
where
    F: Parser<&'a [u8], O1, E>,
    G: Parser<&'a [u8], O2, E>,
{
    preceded(
        preceded(multispace0, block_tag),
        // Add cut around block body in case the block is used in an `alt` as we don't want to
        // try other alternatives if the block_tag matched.
        cut(body),
    )
}

/*
 * Number parsing
 */

/// Parse a number in binary form from a byte array.
pub trait FromBinary
where
    Self: Sized,
{
    fn from_binary<T: ByteOrder>(input: &[u8]) -> IResult<&[u8], Self>;
}

macro_rules! impl_from_binary {
    ($type:ty) => {
        impl FromBinary for $type {
            fn from_binary<T: ByteOrder>(input: &[u8]) -> IResult<&[u8], $type> {
                debug_assert_eq!(::std::mem::size_of::<$type>(), 1);
                if input.len() < 1 {
                    // SAFETY: Called with nonzero value
                    unsafe {
                        Err(nom::Err::Incomplete(Needed::Size(
                            ::std::num::NonZeroUsize::new_unchecked(1),
                        )))
                    }
                } else {
                    Ok((&input[1..], input[0] as $type))
                }
            }
        }
    };
    ($type:ty, $read_fn:ident) => {
        impl FromBinary for $type {
            fn from_binary<T: ByteOrder>(input: &[u8]) -> IResult<&[u8], $type> {
                let size: usize = ::std::mem::size_of::<$type>();
                if input.len() < size {
                    // SAFETY: Can only be called for `size` > 0 as `input.len()` and `size` are both usize
                    unsafe {
                        Err(nom::Err::Incomplete(Needed::Size(
                            ::std::num::NonZeroUsize::new_unchecked(size),
                        )))
                    }
                } else {
                    let res = T::$read_fn(input);
                    Ok((&input[size..], res))
                }
            }
        }
    };
}

impl_from_binary!(u8);
impl_from_binary!(i8);
impl_from_binary!(u16, read_u16);
impl_from_binary!(i16, read_i16);
impl_from_binary!(u32, read_u32);
impl_from_binary!(i32, read_i32);
impl_from_binary!(u64, read_u64);
impl_from_binary!(i64, read_i64);
impl_from_binary!(f32, read_f32);
impl_from_binary!(f64, read_f64);

/// Parse a number in ASCII form from a byte array.
pub trait FromAscii
where
    Self: Sized,
{
    fn from_ascii(input: &[u8]) -> IResult<&[u8], Self>;
}

macro_rules! impl_from_ascii {
    ($type:ty, $fn:ident) => {
        impl FromAscii for $type {
            fn from_ascii(input: &[u8]) -> IResult<&[u8], $type> {
                $fn(input)
            }
        }
    };
}

impl_from_ascii!(u8, unsigned);
impl_from_ascii!(i8, integer);
impl_from_ascii!(u16, unsigned);
impl_from_ascii!(i16, integer);
impl_from_ascii!(u32, unsigned);
impl_from_ascii!(i32, integer);
impl_from_ascii!(u64, unsigned);
impl_from_ascii!(i64, integer);
impl_from_ascii!(f32, real);
impl_from_ascii!(f64, real);

/// Parse a formatted unsigned integer.
pub fn unsigned<T>(input: &[u8]) -> IResult<&[u8], T>
where
    T: FromStr,
{
    map_res(map_res(digit1, std::str::from_utf8), FromStr::from_str)(input)
}

/// Parse a formatted signed integer.
pub fn integer<T>(input: &[u8]) -> IResult<&[u8], T>
where
    T: FromStr,
{
    map_opt(
        recognize(tuple((opt(alt((tag("+"), tag("-")))), digit1))),
        |i: &[u8]| i.parse_to(),
    )(input)
}

/// Parse a floating point number from a byte array.
/// This extends `nom`'s implementation by allowing floats without a decimal point (e.g. `3e3`).
pub fn real<T>(input: &[u8]) -> IResult<&[u8], T>
where
    T: FromStr,
{
    map_opt(
        recognize(tuple((
            opt(alt((tag("+"), tag("-")))),
            alt((
                complete(delimited(digit1, tag("."), opt(digit1))),
                complete(delimited(opt(digit1), tag("."), digit1)),
                complete(digit1),
            )),
            opt(complete(tuple((
                alt((tag("e"), tag("E"))),
                opt(alt((tag("+"), tag("-")))),
                digit1,
            )))),
        ))),
        |i: &[u8]| i.parse_to(),
    )(input)
}

/// A trait identifying all scalar types supported by VTK.
pub trait Scalar: FromStr + FromAscii + FromBinary {}

macro_rules! impl_scalar {
    ($($type:ty),* $(,)*) => {
        $(
            impl Scalar for $type {}
        )*
    }
}

impl_scalar!(u8, i8, u16, i16, u32, i32, u64, i64, f32, f64);

/*
 * Buffer parsing
 */

/// Parse a set of typed numbers into an `IOBuffer`.
pub fn parse_data_buffer<T, BO>(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], IOBuffer>
where
    T: Scalar + Any + Clone + Zero + std::fmt::Debug,
    BO: ByteOrder,
    IOBuffer: From<Vec<T>>,
{
    map(|i| parse_data_vec::<T, BO>(i, n, ft), IOBuffer::from)(input)
}

/// Parse a set of unsigned bytes into an `IOBuffer`.
pub fn parse_data_buffer_u8(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], IOBuffer> {
    map(|i| parse_data_vec_u8(i, n, ft), IOBuffer::from)(input)
}

/// Parse a set of signed bytes into an `IOBuffer`.
pub fn parse_data_buffer_i8(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], IOBuffer> {
    map(|i| parse_data_vec_i8(i, n, ft), IOBuffer::from)(input)
}

/// Parse a set of bits into an `IOBuffer`.
pub fn parse_data_bit_buffer(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], IOBuffer> {
    map(|i| parse_data_bit_vec(i, n, ft), IOBuffer::from)(input)
}

/// Parse a set of typed numbers into a `Vec`.
pub fn parse_data_vec<T, BO>(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<T>>
where
    T: Scalar,
    BO: ByteOrder,
{
    match ft {
        FileType::ASCII => many_m_n(n, n, ws(T::from_ascii))(input),
        FileType::Binary => many_m_n(n, n, T::from_binary::<BO>)(input),
    }
}

/// Parse a set of unsigned bytes into a `Vec`.
pub fn parse_data_vec_u8(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<u8>> {
    match ft {
        FileType::ASCII => many_m_n(n, n, ws(u8::from_ascii))(input),
        FileType::Binary => {
            // If expecting bytes, byte order doesn't matter, just return the entire block.
            if input.len() < n {
                // SAFETY: Can only be called for `n` > 0 as `input.len()` and `n` are both usize
                unsafe {
                    Err(nom::Err::Incomplete(Needed::Size(
                        NonZeroUsize::new_unchecked(n),
                    )))
                }
            } else {
                Ok((&input[n..], input[0..n].to_vec()))
            }
        }
    }
}

/// Parse a set of signed bytes into a `Vec`.
pub fn parse_data_vec_i8(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<i8>> {
    match ft {
        FileType::ASCII => many_m_n(n, n, ws(i8::from_ascii))(input),
        FileType::Binary => {
            // If expecting bytes, byte order doesn't matter, just return the entire block.
            // Unsafety is used here to avoid having to iterate.
            if input.len() < n {
                // SAFETY: Can only be called for `n` > 0 as `input.len()` and `n` are both usize
                unsafe {
                    Err(nom::Err::Incomplete(Needed::Size(
                        NonZeroUsize::new_unchecked(n),
                    )))
                }
            } else {
                // SAFETY: All u8 are representable as i8 and both are 8 bits.
                Ok((
                    &input[n..],
                    unsafe { std::slice::from_raw_parts(input[0..n].as_ptr() as *const i8, n) }
                        .to_vec(),
                ))
            }
        }
    }
}

pub fn parse_data_bit_vec(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<u8>> {
    match ft {
        FileType::ASCII => many_m_n(n, n, ws(u8::from_ascii))(input),
        FileType::Binary => {
            let nbytes: usize = n / 8 + if n % 8 == 0 { 0 } else { 1 };
            if input.len() < nbytes {
                // SAFETY: Can only be called for `nbytes` > 0 as `input.len()` and `nbytes` are both usize
                unsafe {
                    Err(nom::Err::Incomplete(Needed::Size(
                        NonZeroUsize::new_unchecked(nbytes),
                    )))
                }
            } else {
                Ok((&input[nbytes..], input[0..nbytes].to_vec()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use byteorder::BigEndian;

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
        assert_eq!(f, Ok(("".as_bytes(), IOBuffer::from(Vec::<f32>::new()))));
        let f = parse_data_buffer::<f32, BigEndian>("3".as_bytes(), 1, FileType::ASCII);
        assert_eq!(f, Ok(("".as_bytes(), IOBuffer::from(vec![3.0f32]))));
        let f = parse_data_buffer::<f32, BigEndian>("3 32".as_bytes(), 2, FileType::ASCII);
        assert_eq!(f, Ok(("".as_bytes(), IOBuffer::from(vec![3.0f32, 32.0]))));
        let f = parse_data_buffer::<f32, BigEndian>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            Ok((
                "".as_bytes(),
                IOBuffer::from(vec![3.0f32, 32.0, 32.0, 4.0e3])
            ))
        );
        let f = parse_data_buffer::<f64, BigEndian>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            Ok((
                "".as_bytes(),
                IOBuffer::from(vec![3.0f64, 32.0, 32.0, 4.0e3])
            ))
        );
    }
}
