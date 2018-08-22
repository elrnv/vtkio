use std::mem::size_of;
use std::slice;

/// Reinterpret a given slice as a slice of another type. This function checks that the resulting
/// slice is appropriately sized.
pub fn reinterpret_mut_slice<T, S>(slice: &mut [T]) -> &mut [S] {
    // We must be able to split the given slice into appropriately sized chunks.
    assert_eq!((slice.len() * size_of::<T>()) % size_of::<S>(), 0,
                "Slice cannot be safely reinterpreted due to a misaligned size");
    let nu_len = (slice.len() * size_of::<T>()) / size_of::<S>();
    unsafe { slice::from_raw_parts_mut(slice.as_mut_ptr() as *mut S, nu_len) }
}

/// Reinterpret a given slice as a slice of another type. This function checks that the resulting
/// slice is appropriately sized.
pub fn reinterpret_slice<T, S>(slice: &[T]) -> &[S] {
    // We must be able to split the given slice into appropriately sized chunks.
    assert_eq!((slice.len() * size_of::<T>()) % size_of::<S>(), 0,
               "Slice cannot be safely reinterpreted due to a misaligned size");
    let nu_len = (slice.len() * size_of::<T>()) / size_of::<S>();
    unsafe { slice::from_raw_parts(slice.as_ptr() as *const S, nu_len) }
}

/// Reinterpret a given `Vec` as a `Vec` of another type. This function checks that the resulting
/// `Vec` is appropriately sized.
pub fn reinterpret_vec<T, S>(mut vec: Vec<T>) -> Vec<S> {
    // We must be able to split the given vec into appropriately sized chunks.
    assert_eq!((vec.len() * size_of::<T>()) % size_of::<S>(), 0,
               "Vec cannot be safely reinterpreted due to a misaligned size");
    let nu_len = (vec.len() * size_of::<T>()) / size_of::<S>();
    assert_eq!((vec.capacity() * size_of::<T>()) % size_of::<S>(), 0,
               "Vec cannot be safely reinterpreted due to a misaligned capacity");
    let nu_capacity = (vec.capacity() * size_of::<T>()) / size_of::<S>();
    let vec_ptr = vec.as_mut_ptr();
    ::std::mem::forget(vec);
    unsafe { Vec::from_raw_parts(vec_ptr as *mut S, nu_len, nu_capacity) }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check that we can reinterpret a slice of `[f64;3]`s as a slice of `f64`s.
    #[test]
    fn reinterpret_slice_test() {
        let vec: Vec<[f64;3]> = vec![
            [0.1, 1.0, 2.0],
            [1.2, 1.4, 2.1],
            [0.5, 3.2, 4.0],
        ];
        let flat: Vec<f64> = vec![0.1, 1.0, 2.0, 1.2, 1.4, 2.1, 0.5, 3.2, 4.0];
        let nu_flat: &[f64] = reinterpret_slice(vec.as_slice());
        assert_eq!(*nu_flat, *flat.as_slice());

        let nu_slice: &[[f64;3]] = reinterpret_slice(flat.as_slice());
        assert_eq!(*nu_slice, *vec.as_slice());
    }

    #[test]
    fn reinterpret_slice_mut_test() {
        let vec: Vec<[f64;3]> = vec![
            [0.5, 1.0, 2.0],
            [1.2, 1.4, 2.1],
            [0.5, 3.2, 4.0],
        ];
        let flat_mut = &mut [-0.5, -1.0, -1.0, 0.2, -0.6, -0.9, -0.5, 1.2, 1.0];

        let nu_mut_slice: &mut [[f64;3]] = reinterpret_mut_slice(flat_mut);
        for v in nu_mut_slice.iter_mut() {
            v[0] += 1.0;
            v[1] += 2.0;
            v[2] += 3.0;
        }

        assert_eq!(*nu_mut_slice, *vec.as_slice());
    }

    #[test]
    fn reinterpret_vec_test() {
        let exp_vec: Vec<[f64;3]> = vec![
            [0.5, 1.0, 2.0],
            [1.2, 1.4, 2.1],
            [0.5, 3.2, 4.0],
        ];
        let vec = vec![-0.5, -1.0, -1.0, 0.2, -0.6, -0.9, -0.5, 1.2, 1.0];
        let mut nu_vec: Vec<[f64;3]> = reinterpret_vec(vec.clone());
        for v in nu_vec.iter_mut() {
            v[0] += 1.0;
            v[1] += 2.0;
            v[2] += 3.0;
        }

        assert_eq!(nu_vec, exp_vec);
    }
}
