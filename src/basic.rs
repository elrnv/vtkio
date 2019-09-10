use nom::{digit, IResult, Needed};

use crate::IOBuffer;
use byteorder::{ByteOrder, NativeEndian};
use num_traits::Zero;
use std::any::Any;
use std::str::{self, FromStr};

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

/// Parse floating point numbers. This macro works for differnt buffer types.
macro_rules! impl_real_parser {
    ($name:ident ($buf_type:ty)) => {
        pub fn $name<T>(input: &$buf_type) -> IResult<&$buf_type, T>
        where
            T: FromStr,
        {
            flat_map!(
                input,
                recognize!(tuple!(
                    opt!(alt!(tag!("+") | tag!("-"))),
                    alt_complete!(
                        delimited!(digit, tag!("."), opt!(digit))
                            | delimited!(opt!(digit), tag!("."), digit)
                            | digit
                    ),
                    opt!(complete!(tuple!(
                        alt!(tag!("e") | tag!("E")),
                        opt!(alt!(tag!("+") | tag!("-"))),
                        digit
                    )))
                )),
                parse_to!(T)
            )
        }
    };
}

/*
 * Parsing routines
 */

// Consume a spaces and tabs.
named!(pub whitespace, eat_separator!(" \t"));

/// Whitespace separator `sp`. Like `ws` but excludes new-lines.
macro_rules! sp (
    ($i:expr, $($args:tt)*) => ( {
            sep!($i, whitespace, $($args)*)
        })
    );

// Parse a floating point number from a byte array.
// This extends `nom`'s implementation by allowing floats without a decimal point (e.g. `3e3`).
impl_real_parser!(real([u8]));

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
                    IResult::Incomplete(Needed::Size(1))
                } else {
                    IResult::Done(&input[1..], input[0] as $type)
                }
            }
        }
    };
    ($type:ty, $read_fn:ident) => {
        impl FromBinary for $type {
            fn from_binary<T: ByteOrder>(input: &[u8]) -> IResult<&[u8], $type> {
                let size = ::std::mem::size_of::<$type>();
                if input.len() < size {
                    IResult::Incomplete(Needed::Size(size))
                } else {
                    let res = T::$read_fn(input);
                    IResult::Done(&input[size..], res)
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
    map_res!(input, map_res!(digit, str::from_utf8), FromStr::from_str)
}

/// Parse a formatted signed integer.
pub fn integer<T>(input: &[u8]) -> IResult<&[u8], T>
where
    T: FromStr,
{
    flat_map!(
        input,
        recognize!(tuple!(opt!(alt!(tag!("+") | tag!("-"))), digit)),
        parse_to!(T)
    )
}

pub struct DataParser<BO: ByteOrder = NativeEndian>(::std::marker::PhantomData<BO>);

impl<BO: ByteOrder> DataParser<BO> {
    /// Parse a set of typed numbers into an `IOBuffer`.
    pub fn data_buffer<T>(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], IOBuffer>
    where
        T: FromStr + Any + Clone + Zero + FromBinary + FromAscii + ::std::fmt::Debug,
    {
        Self::data_vec::<T>(input, n, ft).map(|vec: Vec<T>| IOBuffer::from(vec))
    }

    /// Parse a set of typed numbers into a `Vec`.
    pub fn data_vec<T>(input: &[u8], n: usize, ft: FileType) -> IResult<&[u8], Vec<T>>
    where
        T: FromStr + FromAscii + FromBinary,
    {
        match ft {
            FileType::ASCII => many_m_n!(input, n, n, ws!(T::from_ascii)),
            FileType::Binary => many_m_n!(input, n, n, T::from_binary::<BO>),
        }
    }
}

#[cfg(test)]
mod tests {
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
        let f = <DataParser>::data_buffer::<f32>("".as_bytes(), 0, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(Vec::<f32>::new()))
        );
        let f = <DataParser>::data_buffer::<f32>("3".as_bytes(), 1, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(vec![3.0f32]))
        );
        let f = <DataParser>::data_buffer::<f32>("3 32".as_bytes(), 2, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done("".as_bytes(), IOBuffer::from(vec![3.0f32, 32.0]))
        );
        let f = <DataParser>::data_buffer::<f32>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "".as_bytes(),
                IOBuffer::from(vec![3.0f32, 32.0, 32.0, 4.0e3])
            )
        );
        let f = <DataParser>::data_buffer::<f64>("3 32 32.0 4e3".as_bytes(), 4, FileType::ASCII);
        assert_eq!(
            f,
            IResult::Done(
                "".as_bytes(),
                IOBuffer::from(vec![3.0f64, 32.0, 32.0, 4.0e3])
            )
        );
    }
}
