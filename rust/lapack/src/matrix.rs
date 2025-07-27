//! Matrix and Vector types for safe LAPACK operations
//!
//! This module provides high-performance matrix and vector types optimized
//! for LAPACK operations. Key features include:
//! 
//! - **Layout flexibility**: Support for both row-major and column-major layouts
//! - **Zero-copy layout conversion**: In-place transposition for large matrices
//! - **Safe and unsafe APIs**: Bounds-checked methods with unchecked variants for performance
//! - **LAPACK compatibility**: Direct use with LAPACK FFI bindings
//!
//! # Performance Considerations
//!
//! For optimal performance:
//! - Use `get_unchecked` / `get_unchecked_mut` in tight loops after validating bounds
//! - Prefer column-major layout for LAPACK operations (avoids conversion)
//! - Use `convert_layout` for in-place layout changes on large matrices
//!
//! # Examples
//!
//! ```rust
//! use lapack::{Matrix, Layout};
//! 
//! // Create a 3x3 matrix in row-major layout
//! let mut mat = Matrix::from_slice(&[1.0, 2.0, 3.0,
//!                                    4.0, 5.0, 6.0,
//!                                    7.0, 8.0, 9.0], 3, 3, Layout::RowMajor)?;
//!
//! // Convert to column-major for LAPACK (in-place for large matrices)
//! mat.convert_layout(Layout::ColumnMajor);
//!
//! // Safe element access
//! let val = mat.get(1, 2)?;
//! 
//! // Unsafe element access for performance
//! unsafe {
//!     let fast_val = mat.get_unchecked(1, 2);
//! }
//! # Ok::<(), lapack::Error>(())
//! ```

use crate::{Error, Layout, Result};
use std::fmt;

/// A two-dimensional matrix with automatic layout management
#[derive(Debug, Clone, PartialEq)]
pub struct Matrix<T> {
    /// Matrix data stored as a contiguous vector
    data: Vec<T>,
    /// Number of rows
    rows: usize,
    /// Number of columns  
    cols: usize,
    /// Data layout (row-major or column-major)
    layout: Layout,
}

/// A one-dimensional vector
#[derive(Debug, Clone, PartialEq)]
pub struct Vector<T> {
    /// Vector data
    data: Vec<T>,
}

impl<T: Clone + Default> Matrix<T> {
    /// Create a new matrix with specified dimensions and layout
    pub fn new(rows: usize, cols: usize, layout: Layout) -> Self {
        let data = vec![T::default(); rows * cols];
        Self { data, rows, cols, layout }
    }

    /// Create a matrix filled with zeros
    pub fn zeros(rows: usize, cols: usize, layout: Layout) -> Result<Self> {
        Ok(Self::new(rows, cols, layout))
    }

    /// Create a matrix from existing data with specified layout
    pub fn from_vec(data: Vec<T>, rows: usize, cols: usize, layout: Layout) -> Result<Self> {
        if data.len() != rows * cols {
            return Err(Error::DimensionMismatch {
                expected: rows * cols,
                actual: data.len(),
            });
        }
        Ok(Self { data, rows, cols, layout })
    }

    /// Create a matrix from a slice with specified layout
    pub fn from_slice(data: &[T], rows: usize, cols: usize, layout: Layout) -> Result<Self> {
        Self::from_vec(data.to_vec(), rows, cols, layout)
    }

    /// Get the number of rows
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// Get the number of columns
    pub fn cols(&self) -> usize {
        self.cols
    }

    /// Get the matrix layout
    pub fn layout(&self) -> Layout {
        self.layout
    }

    /// Get the underlying data as a slice
    pub fn as_slice(&self) -> &[T] {
        &self.data
    }

    /// Get the underlying data as a mutable slice
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.data
    }

    /// Convert to raw data vector, consuming the matrix
    pub fn into_raw(self) -> Vec<T> {
        self.data
    }

    /// Get a reference to the underlying data vector
    pub fn data(&self) -> &Vec<T> {
        &self.data
    }

    /// Get a mutable reference to the underlying data vector
    pub fn data_mut(&mut self) -> &mut Vec<T> {
        &mut self.data
    }

    /// Get matrix element at (row, col)
    pub fn get(&self, row: usize, col: usize) -> Result<&T> {
        if row >= self.rows || col >= self.cols {
            return Err(Error::DimensionMismatch {
                expected: self.rows * self.cols,
                actual: row * self.cols + col + 1,
            });
        }

        let index = match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        };

        Ok(&self.data[index])
    }
    
    /// Get matrix element at (row, col) without bounds checking
    /// 
    /// # Safety
    /// Caller must ensure that row < self.rows and col < self.cols
    #[inline]
    pub unsafe fn get_unchecked(&self, row: usize, col: usize) -> &T {
        let index = match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        };
        self.data.get_unchecked(index)
    }

    /// Set matrix element at (row, col)
    pub fn set(&mut self, row: usize, col: usize, value: T) -> Result<()> {
        if row >= self.rows || col >= self.cols {
            return Err(Error::DimensionMismatch {
                expected: self.rows * self.cols,
                actual: row * self.cols + col + 1,
            });
        }

        let index = match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        };

        self.data[index] = value;
        Ok(())
    }

    /// Get a mutable reference to the element at (row, col)
    pub fn get_mut(&mut self, row: usize, col: usize) -> Result<&mut T> {
        if row >= self.rows || col >= self.cols {
            return Err(Error::DimensionMismatch {
                expected: self.rows * self.cols,
                actual: row * self.cols + col + 1,
            });
        }

        let index = match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        };

        Ok(&mut self.data[index])
    }
    
    /// Get a mutable reference to element at (row, col) without bounds checking
    /// 
    /// # Safety
    /// Caller must ensure that row < self.rows and col < self.cols
    #[inline]
    pub unsafe fn get_unchecked_mut(&mut self, row: usize, col: usize) -> &mut T {
        let index = match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        };
        self.data.get_unchecked_mut(index)
    }

    /// Check if the matrix has the same dimensions as another matrix
    pub fn same_dimensions<U>(&self, other: &Matrix<U>) -> bool {
        self.rows == other.rows && self.cols == other.cols
    }

    /// Check if this matrix can be multiplied by another matrix (A * B)
    pub fn can_multiply<U>(&self, other: &Matrix<U>) -> bool {
        self.cols == other.rows
    }
}

impl<T: Clone> Matrix<T> {
    /// Convert matrix to row-major layout
    pub fn to_row_major(&self) -> Self {
        if self.layout == Layout::RowMajor {
            // Already row-major, just clone
            return self.clone();
        }

        // Convert from column-major to row-major
        let mut new_data = Vec::with_capacity(self.data.len());
        for row in 0..self.rows {
            for col in 0..self.cols {
                let col_major_index = col * self.rows + row;
                new_data.push(self.data[col_major_index].clone());
            }
        }

        Matrix {
            data: new_data,
            rows: self.rows,
            cols: self.cols,
            layout: Layout::RowMajor,
        }
    }

    /// Convert matrix to column-major layout
    pub fn to_column_major(&self) -> Self {
        if self.layout == Layout::ColumnMajor {
            // Already column-major, just clone
            return self.clone();
        }

        // Convert from row-major to column-major
        let mut new_data = Vec::with_capacity(self.data.len());
        for col in 0..self.cols {
            for row in 0..self.rows {
                let row_major_index = row * self.cols + col;
                new_data.push(self.data[row_major_index].clone());
            }
        }

        Matrix {
            data: new_data,
            rows: self.rows,
            cols: self.cols,
            layout: Layout::ColumnMajor,
        }
    }

    /// Convert matrix layout in place (mutating version)
    ///
    /// This method efficiently converts between row-major and column-major layouts.
    /// For small matrices (< 64 elements), it uses a simple allocation-based approach.
    /// For larger matrices, it performs in-place transposition to avoid memory allocation.
    ///
    /// # Performance
    /// 
    /// - Small matrices: O(n) time, O(n) space
    /// - Large matrices: O(n) time, O(1) space (plus temporary cycle tracking)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use lapack::{Matrix, Layout};
    /// 
    /// let mut mat = Matrix::from_slice(&[1.0, 2.0, 3.0, 4.0], 2, 2, Layout::RowMajor)?;
    /// mat.convert_layout(Layout::ColumnMajor);
    /// assert_eq!(mat.layout(), Layout::ColumnMajor);
    /// # Ok::<(), lapack::Error>(())
    /// ```
    pub fn convert_layout(&mut self, target_layout: Layout) {
        if self.layout == target_layout {
            return;
        }

        // For small matrices, use the simple allocation-based approach
        if self.data.len() < 64 {
            *self = match target_layout {
                Layout::RowMajor => self.to_row_major(),
                Layout::ColumnMajor => self.to_column_major(),
            };
            return;
        }

        // For larger matrices, use in-place transposition
        self.transpose_in_place();
        self.layout = target_layout;
    }

    /// In-place matrix transposition using cycle-following algorithm
    /// This avoids allocating a new vector for the transposed data
    fn transpose_in_place(&mut self) {
        if self.rows == self.cols {
            // Square matrix - use simple swapping
            for i in 0..self.rows {
                for j in i+1..self.cols {
                    let idx1 = self.index_for(i, j);
                    let idx2 = self.index_for(j, i);
                    self.data.swap(idx1, idx2);
                }
            }
        } else {
            // Non-square matrix - use cycle-following algorithm
            let size = self.data.len();
            let mut visited = vec![false; size];
            
            for start in 1..size-1 {
                if visited[start] {
                    continue;
                }
                
                let mut current = start;
                let temp = self.data[start].clone();
                
                loop {
                    let next = self.transpose_index(current);
                    visited[current] = true;
                    
                    if next == start {
                        self.data[current] = temp.clone();
                        break;
                    } else {
                        self.data[current] = self.data[next].clone();
                        current = next;
                    }
                }
            }
            
            // Swap dimensions for the transpose
            std::mem::swap(&mut self.rows, &mut self.cols);
        }
    }

    /// Calculate the linear index for element at (row, col)
    #[inline]
    fn index_for(&self, row: usize, col: usize) -> usize {
        match self.layout {
            Layout::RowMajor => row * self.cols + col,
            Layout::ColumnMajor => col * self.rows + row,
        }
    }

    /// Calculate the transposed index for in-place transposition
    #[inline]
    fn transpose_index(&self, idx: usize) -> usize {
        let (row, col) = match self.layout {
            Layout::RowMajor => (idx / self.cols, idx % self.cols),
            Layout::ColumnMajor => (idx % self.rows, idx / self.rows),
        };
        
        match self.layout {
            Layout::RowMajor => col * self.rows + row,
            Layout::ColumnMajor => row * self.cols + col,
        }
    }
}

impl<T: Clone + Default> Vector<T> {
    /// Create a new vector with specified length
    pub fn new(len: usize) -> Self {
        let data = vec![T::default(); len];
        Self { data }
    }

    /// Create a vector filled with zeros
    pub fn zeros(len: usize) -> Self {
        Self::new(len)
    }

    /// Create a vector from existing data
    pub fn from_vec(data: Vec<T>) -> Self {
        Self { data }
    }

    /// Create a vector from a slice
    pub fn from_slice(data: &[T]) -> Self {
        Self::from_vec(data.to_vec())
    }

    /// Get the length of the vector
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get the underlying data as a slice
    pub fn as_slice(&self) -> &[T] {
        &self.data
    }

    /// Get the underlying data as a mutable slice
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut self.data
    }

    /// Convert to raw data vector, consuming the vector
    pub fn into_raw(self) -> Vec<T> {
        self.data
    }

    /// Get a reference to the underlying data vector
    pub fn data(&self) -> &Vec<T> {
        &self.data
    }

    /// Get a mutable reference to the underlying data vector
    pub fn data_mut(&mut self) -> &mut Vec<T> {
        &mut self.data
    }

    /// Get vector element at index
    pub fn get(&self, index: usize) -> Result<&T> {
        self.data.get(index).ok_or(Error::DimensionMismatch {
            expected: self.data.len(),
            actual: index + 1,
        })
    }
    
    /// Get vector element at index without bounds checking
    /// 
    /// # Safety
    /// Caller must ensure that index < self.len()
    #[inline]
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        self.data.get_unchecked(index)
    }

    /// Set vector element at index
    pub fn set(&mut self, index: usize, value: T) -> Result<()> {
        if index >= self.data.len() {
            return Err(Error::DimensionMismatch {
                expected: self.data.len(),
                actual: index + 1,
            });
        }
        self.data[index] = value;
        Ok(())
    }

    /// Get a mutable reference to the element at index
    pub fn get_mut(&mut self, index: usize) -> Result<&mut T> {
        let len = self.data.len();
        self.data.get_mut(index).ok_or(Error::DimensionMismatch {
            expected: len,
            actual: index + 1,
        })
    }
    
    /// Get a mutable reference to element at index without bounds checking
    /// 
    /// # Safety
    /// Caller must ensure that index < self.len()
    #[inline]
    pub unsafe fn get_unchecked_mut(&mut self, index: usize) -> &mut T {
        self.data.get_unchecked_mut(index)
    }
}

// Implement Display for Matrix<f64> for easy debugging
impl fmt::Display for Matrix<f64> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Matrix {}x{} ({:?}):", self.rows, self.cols, self.layout)?;
        for row in 0..self.rows {
            write!(f, "[")?;
            for col in 0..self.cols {
                if let Ok(value) = self.get(row, col) {
                    if col > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:8.4}", value)?;
                }
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

// Implement Display for Vector<f64> for easy debugging
impl fmt::Display for Vector<f64> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vector[{}]: [", self.len())?;
        for (i, value) in self.data.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:8.4}", value)?;
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matrix_creation() {
        let m = Matrix::<f64>::new(2, 3, Layout::RowMajor);
        assert_eq!(m.rows(), 2);
        assert_eq!(m.cols(), 3);
        assert_eq!(m.layout(), Layout::RowMajor);
        assert_eq!(m.as_slice().len(), 6);
    }

    #[test]
    fn test_matrix_from_vec() {
        let data = vec![1.0, 2.0, 3.0, 4.0];
        let m = Matrix::from_vec(data, 2, 2, Layout::RowMajor).unwrap();
        assert_eq!(m.get(0, 0).unwrap(), &1.0);
        assert_eq!(m.get(0, 1).unwrap(), &2.0);
        assert_eq!(m.get(1, 0).unwrap(), &3.0);
        assert_eq!(m.get(1, 1).unwrap(), &4.0);
    }

    #[test]
    fn test_matrix_layout_conversion() {
        let data = vec![1.0, 2.0, 3.0, 4.0]; // [1,2; 3,4] in row-major
        let m = Matrix::from_vec(data, 2, 2, Layout::RowMajor).unwrap();
        
        let col_major = m.to_column_major();
        assert_eq!(col_major.layout(), Layout::ColumnMajor);
        // Column-major should be [1, 3, 2, 4]
        assert_eq!(col_major.as_slice(), &[1.0, 3.0, 2.0, 4.0]);
        
        let back_to_row = col_major.to_row_major();
        assert_eq!(back_to_row.as_slice(), &[1.0, 2.0, 3.0, 4.0]);
    }

    #[test]
    fn test_vector_creation() {
        let v = Vector::<f64>::new(5);
        assert_eq!(v.len(), 5);
        assert!(!v.is_empty());
    }

    #[test]
    fn test_vector_from_vec() {
        let data = vec![1.0, 2.0, 3.0];
        let v = Vector::from_vec(data);
        assert_eq!(v.get(0).unwrap(), &1.0);
        assert_eq!(v.get(1).unwrap(), &2.0);
        assert_eq!(v.get(2).unwrap(), &3.0);
    }

    #[test]
    fn test_matrix_element_access() {
        let mut m = Matrix::<f64>::new(2, 2, Layout::RowMajor);
        m.set(0, 0, 1.0).unwrap();
        m.set(0, 1, 2.0).unwrap();
        m.set(1, 0, 3.0).unwrap();
        m.set(1, 1, 4.0).unwrap();

        assert_eq!(m.get(0, 0).unwrap(), &1.0);
        assert_eq!(m.get(0, 1).unwrap(), &2.0);
        assert_eq!(m.get(1, 0).unwrap(), &3.0);
        assert_eq!(m.get(1, 1).unwrap(), &4.0);
    }

    #[test]
    fn test_bounds_checking() {
        let m = Matrix::<f64>::new(2, 2, Layout::RowMajor);
        assert!(m.get(2, 0).is_err());
        assert!(m.get(0, 2).is_err());

        let v = Vector::<f64>::new(3);
        assert!(v.get(3).is_err());
    }
}