//! Utility macros for code generation.

#![macro_use]

/// Applies `$fn` to an `IOBuffer` mapping valid numeric data types by corresponding generic
/// parameters.  For example, passing an `IOBuffer` containing data of type `u8` will cause this
/// macro to call `$fn` with type parameter `u8` like `$fn::<u8>(io_buffer)`.
/// # Examples
/// ```rust,ignore
/// # #[macro_use] extern crate meshio;
/// # use std::fmt;
/// # use std::any::Any;
/// # use meshio::buffer::IOBuffer;
/// # fn main() {
/// // Implement pretty printing of `IOBuffer` contents.
/// impl fmt::Display for IOBuffer {
///     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
///         fn display_buf<T: Any + fmt::Display>(buf: &IOBuffer, f: &mut fmt::Formatter) {
///             for item in buf.iter::<T>() {
///                 write!(f, "{} ", item)
///                     .expect("Error occurred while writing an IOBuffer.");
///             }
///         }
///         call_numeric_fn!( display_buf::<_>(self, f) or {});
///         write!(f, "")
///     }
/// }
/// # }
/// ```
#[macro_export]
macro_rules! call_numeric_fn {
    ($fn:ident ::<_,$($params:ident),*>( $data:ident, $($args:expr),* ) or $err:block ) => {
        match $data.type_id() {
            x if x == TypeId::of::<u8>() =>  $fn::<u8,$($params),*> ($data, $($args),*),
            x if x == TypeId::of::<i8>() =>  $fn::<i8,$($params),*> ($data, $($args),*),
            x if x == TypeId::of::<u16>() => $fn::<u16,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<i16>() => $fn::<i16,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<u32>() => $fn::<u32,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<i32>() => $fn::<i32,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<u64>() => $fn::<u64,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<i64>() => $fn::<i64,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<f32>() => $fn::<f32,$($params),*>($data, $($args),*),
            x if x == TypeId::of::<f64>() => $fn::<f64,$($params),*>($data, $($args),*),
            _ => $err,
        }
    };
    // Same thing as above but with one parameter argument.
    ($fn:ident ::<_>( $($args:expr),* ) or $err:block ) => {
        call_numeric_fn!($fn ::<_,>( $($args),* ) or $err )
    };
    // Same thing as above but with one function argument.
    ($fn:ident ::<_,$($params:ident),*>( $data:ident ) or $err:block ) => {
        call_numeric_fn!($fn ::<_,$($params),*>( $data, ) or $err )
    };
    // Using method synax for member functions if any.
    ($data:ident . $fn:ident ::<_,$($params:ident),*>( $($args:expr),* ) or $err:block ) => {
        match $data.type_id() {
            x if x == TypeId::of::<u8>() =>  $data.$fn::<u8,$($params),*> ($($args),*),
            x if x == TypeId::of::<i8>() =>  $data.$fn::<i8,$($params),*> ($($args),*),
            x if x == TypeId::of::<u16>() => $data.$fn::<u16,$($params),*>($($args),*),
            x if x == TypeId::of::<i16>() => $data.$fn::<i16,$($params),*>($($args),*),
            x if x == TypeId::of::<u32>() => $data.$fn::<u32,$($params),*>($($args),*),
            x if x == TypeId::of::<i32>() => $data.$fn::<i32,$($params),*>($($args),*),
            x if x == TypeId::of::<u64>() => $data.$fn::<u64,$($params),*>($($args),*),
            x if x == TypeId::of::<i64>() => $data.$fn::<i64,$($params),*>($($args),*),
            x if x == TypeId::of::<f32>() => $data.$fn::<f32,$($params),*>($($args),*),
            x if x == TypeId::of::<f64>() => $data.$fn::<f64,$($params),*>($($args),*),
            _ => $err,
        }
    };
    // Same as above but with one parameter argument.
    ($data:ident . $fn:ident ::<_>( $($args:expr),* ) or $err:block ) => {
        call_numeric_fn!($data . $fn ::<_,>( $($args),* ) or $err )
    };
}

