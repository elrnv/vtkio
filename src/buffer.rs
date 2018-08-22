//! This module defines a buffer data structure optimized to be written to and
//! read from standard `Vec`s. `IOBuffer` is particularly useful when reading
//! data whose type is determined at run time.
//! It's also easy to pass this buffer around since it has the same size for all
//! types stored within.

use num_traits::{cast, NumCast, Zero};
use std::any::{Any, TypeId};
use std::mem::size_of;
use std::fmt;
use std::slice;
use reinterpret;

/// Buffer struct used to store typed data efficiently without requiring the type to be available
/// at compile time. This makes it easy to pass around data and reinterpret it.
/// This buffer stores underlying data in native bye order.
#[derive(Clone, PartialEq, Debug)]
pub struct IOBuffer {
    /// Raw data stored as an array of bytes.
    data: Vec<u8>,
    /// Number of type sized chunks in the buffer.
    length: usize,
    /// Type encoding for hiding the type of the data from the compiler.
    type_id: TypeId,
}

impl IOBuffer {
    /// Construct an empty `IOBuffer`
    pub fn new() -> Self {
        IOBuffer {
            data: Vec::new(),
            length: 0,
            type_id: TypeId::of::<()>(),
        }
    }

    /// Construct a typed `IOBuffer` with a given size. This buffer stores its data in 8 bit
    /// chunks.
    pub fn with_capacity<T: Any>(n: usize) -> Self {
        let num_bytes = n * size_of::<T>();
        IOBuffer {
            data: Vec::with_capacity(num_bytes),
            length: n,
            type_id: TypeId::of::<T>(),
        }
    }

    /// Construct an `IOBuffer` from a given `Vec<T>` reusing the space already allocated by the
    /// given vector.
    pub fn from_vec<T: Any>(mut vec: Vec<T>) -> Self {
        let length = vec.len();

        let data = {
            let len_in_bytes = length * size_of::<T>();
            let capacity_in_bytes = vec.capacity() * size_of::<T>();
            vec.shrink_to_fit();
            let vec_ptr = vec.as_mut_ptr() as *mut u8;

            unsafe {
                ::std::mem::forget(vec);
                Vec::from_raw_parts(vec_ptr, len_in_bytes, capacity_in_bytes)
            }
        };

        IOBuffer {
            data,
            length,
            type_id: TypeId::of::<T>(),
        }
    }

    /// Peak at the internal representation of the data.
    #[inline]
    pub fn raw_data(&self) -> &Vec<u8> {
        &self.data
    }

    /// Get a mutable reference to the internal data representation.
    #[inline]
    pub fn raw_mut_data(&mut self) -> &mut Vec<u8> {
        &mut self.data
    }

    /// Get the `TypeId` of the data stored within this buffer.
    #[inline]
    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    /// Clear the buffer and set length to zero. This call also sets the internal type to `()`.
    #[allow(dead_code)]
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
        self.length = 0;
        self.type_id = TypeId::of::<()>();
    }

    /// Get the number of elements stored in this buffer.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Copy data from a given slice into the current buffer.
    pub fn copy_from_slice<T: Any>(&mut self, slice: &[T]) -> &mut Self {
        let length = slice.len();
        let bins = length * size_of::<T>();
        let byte_slice = unsafe { slice::from_raw_parts(slice.as_ptr() as *const u8, bins) };
        self.data.resize(bins, 0);
        self.data.copy_from_slice(byte_slice);
        self.length = length;
        self.type_id = TypeId::of::<T>();
        self
    }

    /// Move buffer data to a vector with a given type, reinterpreting the data type as
    /// required.
    pub fn reinterpret_as_vec<T>(self) -> Vec<T> {
        reinterpret::reinterpret_vec(self.data)
    }

    /// Borrow buffer data and reinterpret it as a slice of a given type.
    pub fn reinterpret_as_slice<T>(&self) -> &[T] {
        reinterpret::reinterpret_slice(self.data.as_slice())
    }

    /// Mutably borrow buffer data and reinterpret it as a mutable slice of a given type.
    pub fn reinterpret_as_mut_slice<T>(&mut self) -> &mut [T] {
        reinterpret::reinterpret_mut_slice(self.data.as_mut_slice())
    }

    /// Borrow buffer data and iterate over reinterpreted underlying data.
    pub fn reinterpret_iter<T>(&self) -> slice::Iter<T> {
        self.reinterpret_as_slice().iter()
    }

    /// Mutably borrow buffer data and mutably iterate over reinterpreted underlying data.
    pub fn reinterpret_iter_mut<T>(&mut self) -> slice::IterMut<T> {
        self.reinterpret_as_mut_slice().iter_mut()
    }

    /// Check if the current buffer contains elements of the specified type.
    #[inline]
    pub fn check<T: Any>(self) -> Option<Self> {
        if TypeId::of::<T>() != self.type_id() {
            None
        } else {
            Some(self)
        }
    }

    /// Check if the current buffer contains elements of the specified type.
    #[inline]
    pub fn check_ref<T: Any>(&self) -> Option<&Self> {
        if TypeId::of::<T>() != self.type_id() {
            None
        } else {
            Some(self)
        }
    }

    /// Check if the current buffer contains elements of the specified type. This is a mutable
    /// version of `check`.
    #[inline]
    pub fn check_mut<T: Any>(&mut self) -> Option<&mut Self> {
        if TypeId::of::<T>() != self.type_id() {
            None
        } else {
            Some(self)
        }
    }

    /// Move buffer data to a `Vec`. This function performs a runtime check to ensure the stored
    /// type is the same as the requested output type. Return `None` in case of type mismatch.
    pub fn into_vec<T: Any>(self) -> Option<Vec<T>> {
        self.check::<T>().map(|x| x.reinterpret_as_vec())
    }

    /// Convert this buffer to a slice. This function performs a runtime check to ensure the stored
    /// type is the same as the requested output type.
    pub fn as_slice<T: Any>(&self) -> Option<&[T]> {
        self.check_ref::<T>().map(|x| x.reinterpret_as_slice())
    }

    /// Convert this buffer to a mutable slice. This function performs a runtime check to ensure
    /// the stored type is the same as the requested output type.
    pub fn as_mut_slice<T: Any>(&mut self) -> Option<&mut [T]> {
        self.check_mut::<T>().map(|x| x.reinterpret_as_mut_slice())
    }

    /// Append a copy of the data from this buffer into the given `Vec` and return back a mutable
    /// reference to the same vector for convenience. Return `None` in case of a type mismatch.
    /// This function may be faster than `append_clone_to_vec`.
    pub fn append_copy_to_vec<'a, T>(&self, vec: &'a mut Vec<T>) -> Option<&'a mut Vec<T>>
        where T: Any + Copy
    {
        vec.extend(self.as_slice()?);
        Some(vec)
    }

    /// Append a clone of the data from this buffer into the given `Vec` and return back a mutable
    /// reference to the same vector for convenience. Return `None` in case of a type mismatch.
    pub fn append_clone_to_vec<'a, T>(&self, vec: &'a mut Vec<T>) -> Option<&'a mut Vec<T>>
        where T: Any + Clone
    {
        vec.extend_from_slice(self.as_slice()?);
        Some(vec)
    }

    /// Return an iterator to the underlying typed data. Return `None` in case of type mismatch.
    #[inline]
    pub fn iter<'a, T: Any + 'a>(&'a self) -> Option<slice::Iter<T>> {
        self.as_slice::<T>().map(|x| x.iter())
    }

    /// Return a mutable iterator to the underlying typed data. Return `None` in case of type
    /// mismatch.
    #[inline]
    pub fn iter_mut<'a, T: Any + 'a>(&'a mut self) -> Option<slice::IterMut<T>> {
        self.as_mut_slice::<T>().map(|x| x.iter_mut())
    }

    /// Cast a numeric `IOBuffer` into the given output `Vec` type.
    pub fn cast_into_vec<T>(self) -> Vec<T>
        where T: Any + Copy + NumCast + Zero
    {
        // Helper function (generic on the input) to conver the given IOBuffer into Vec.
        fn convert_into_vec<I,O>(buf: IOBuffer) -> Vec<O>
            where I: Any + NumCast,
                  O: Any + Copy + NumCast + Zero
        {
            debug_assert_eq!(buf.type_id(), TypeId::of::<I>()); // Check invariant.
            buf.reinterpret_as_vec()
               .into_iter()
               .map(|elem: I| cast(elem).unwrap_or(O::zero())).collect()
        }
        call_numeric_fn!( convert_into_vec::<_,T>(self) or { Vec::new() } )
    }

    fn display_buf<T: Any + fmt::Display>(&self, f: &mut fmt::Formatter) {
        debug_assert_eq!(self.type_id(), TypeId::of::<T>()); // Check invariant.
        for item in self.reinterpret_iter::<T>() {
            write!(f, "{} ", item)
                .expect("Error occurred while writing an IOBuffer.");
        }
    }
}

/// Convert a `Vec<T>` to an `IOBuffer`.
impl<T> From<Vec<T>> for IOBuffer where T: Any {
    fn from(vec: Vec<T>) -> IOBuffer {
        IOBuffer::from_vec(vec)
    }
}

/// Convert an `IOBuffer` to a `Option<Vec<T>>`. This conversion returns `None` in case of a type
/// mismatch.
impl<T> Into<Option<Vec<T>>> for IOBuffer where T: Any + Clone {
    fn into(self) -> Option<Vec<T>> {
        self.into_vec()
    }
}

/// Implement pretty printing of `IOBuffer` contents for numeric data.
impl fmt::Display for IOBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        call_numeric_fn!( self.display_buf::<_>(f) or {} );
        write!(f, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn data_integrity_u8_test() {
        let vec = vec![1u8, 3, 4, 1, 2];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec = buf.into_vec::<u8>().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);

        let vec = vec![1u8, 3, 4, 1, 2, 52, 1, 3, 41, 23, 2];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Option<Vec<u8>> = buf.into(); // Convert back into vec
        assert!(nu_vec.is_some());
        assert_eq!(vec, nu_vec.unwrap());
    }

    #[test] fn data_integrity_i16_test() {
        let vec = vec![1i16, -3, 1002, -231, 32];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<i16> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);

        let vec = vec![1i16, -3, 1002, -231, 32, 42, -123, 4];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<i16> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);
    }

    #[test] fn data_integrity_i32_test() {
        let vec = vec![1i32, -3, 1002, -231, 32];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<i32> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);

        let vec = vec![1i32, -3, 1002, -231, 32, 42, -123];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<i32> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);
    }

    #[test] fn data_integrity_f32_test() {
        let vec = vec![1.0_f32, 23.0, 0.01, 42.0, 11.43];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<f32> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);

        let vec = vec![1.0_f32, 23.0, 0.01, 42.0, 11.43, 2e-1];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<f32> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);
    }

    #[test] fn data_integrity_f64_test() {
        let vec = vec![1f64, -3.0, 10.02, -23.1, 32e-1];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<f64> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);

        let vec = vec![1f64, -3.1, 100.2, -2.31, 3.2, 4e2, -1e23];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        let nu_vec: Vec<f64> = buf.into_vec().unwrap(); // Convert back into vec
        assert_eq!(vec, nu_vec);
    }

    #[test] fn convert_float_test() {
        let vecf64 = vec![1f64, -3.0, 10.02, -23.1, 32e-1];
        let buf = IOBuffer::from(vecf64.clone()); // Convert into buffer
        let nu_vec: Vec<f32> = buf.cast_into_vec(); // Convert back into vec
        let vecf32 = vec![1f32, -3.0, 10.02, -23.1, 32e-1];
        assert_eq!(vecf32, nu_vec);

        let buf = IOBuffer::from(vecf32.clone()); // Convert into buffer
        let nu_vec: Vec<f64> = buf.cast_into_vec(); // Convert back into vec
        for (&a, &b) in vecf64.iter().zip(nu_vec.iter()) {
            assert!((a - b).abs() < 1e-6f64*f64::max(a,b).abs());
        }

        let vecf64 = vec![1f64, -3.1, 100.2, -2.31, 3.2, 4e2, -1e23];
        let buf = IOBuffer::from(vecf64.clone()); // Convert into buffer
        let nu_vec: Vec<f32> = buf.cast_into_vec(); // Convert back into vec
        let vecf32 = vec![1f32, -3.1, 100.2, -2.31, 3.2, 4e2, -1e23];
        assert_eq!(vecf32, nu_vec);
        let buf = IOBuffer::from(vecf32.clone()); // Convert into buffer
        let nu_vec: Vec<f64> = buf.cast_into_vec(); // Convert back into vec
        for (&a, &b) in vecf64.iter().zip(nu_vec.iter()) {
            assert!((a - b).abs() < 1e-6*f64::max(a,b).abs());
        }
    }

    #[test] fn iter_test() {
        // Check iterating over data with the same size
        let vec = vec![1.0_f32, 23.0, 0.01, 42.0, 11.43];
        let buf = IOBuffer::from(vec.clone()); // Convert into buffer
        for (i, &val) in buf.iter::<f32>().unwrap().enumerate() {
            assert_eq!(val, vec[i]);
        }
    }

    #[test] fn reinterpret_iter_test() {
        // Check iterating over data with a larger size than input
        let vec_u8 = vec![1u8, 3, 4, 1, 2, 4, 128, 32];
        let vec_u32 = vec![17_040_129u32, 545_260_546]; // little endian
        let buf = IOBuffer::from(vec_u8.clone()); // Convert into buffer
        for (i, &val) in buf.reinterpret_iter::<u32>().enumerate() {
            assert_eq!(val, vec_u32[i]);
        }

        // Check iterating over data with a smaller size than input
        let mut buf2 = IOBuffer::from(vec_u32); // Convert into buffer
        for (i, &val) in buf2.reinterpret_iter::<u8>().enumerate() {
            assert_eq!(val, vec_u8[i]);
        }

        // Check mut iterator
        for val in buf2.reinterpret_iter_mut::<u8>() {
            *val += 1;
        }
        let u8_check: Vec<u32> = IOBuffer::from(vec![2u8, 4, 5, 2, 3, 5, 129, 33]).reinterpret_as_vec();
        let buf2_vec: Vec<u32> = buf2.into_vec().unwrap();
        assert_eq!(buf2_vec, u8_check);
    }

    #[test] fn large_sizes_test() {
        for i in 1000000..1000010 {
            let vec = vec![32u8; i];
            let buf = IOBuffer::from(vec.clone()); // Convert into buffer
            let nu_vec: Vec<u8> = buf.into_vec().unwrap(); // Convert back into vec
            assert_eq!(vec, nu_vec);
        }
    }

}
