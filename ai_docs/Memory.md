# Fortran Memory Management

## Dynamic Memory Allocation

### Allocatable Arrays

Most common method for dynamic memory in Fortran.

```fortran
! Declaration
REAL, ALLOCATABLE :: vector(:)
INTEGER, ALLOCATABLE :: matrix(:,:)
TYPE(particle), ALLOCATABLE :: particles(:)

! Allocation
ALLOCATE(vector(n))
ALLOCATE(matrix(rows, cols), STAT=ierr)
IF (ierr /= 0) STOP "Allocation failed"

! Deallocation
DEALLOCATE(vector)
DEALLOCATE(matrix, STAT=ierr)

! Check allocation status
IF (ALLOCATED(vector)) THEN
  ! Use vector
END IF
```

### Allocatable Scalars (F2003)
```fortran
CHARACTER(:), ALLOCATABLE :: string
ALLOCATE(CHARACTER(LEN=100) :: string)
string = "Dynamic string"
```

### Automatic Reallocation (F2003)
```fortran
REAL, ALLOCATABLE :: array(:)
array = [1.0, 2.0, 3.0]  ! Automatic allocation
array = [1.0, 2.0, 3.0, 4.0, 5.0]  ! Automatic reallocation
```

## Pointers

More flexible but require careful management.

### Pointer Basics
```fortran
! Declaration
REAL, POINTER :: ptr
REAL, POINTER :: array_ptr(:)
TYPE(node), POINTER :: list_ptr

! Target declaration
REAL, TARGET :: x = 3.14
REAL, TARGET :: data(100)

! Pointer assignment
ptr => x
array_ptr => data
```

### Dynamic Allocation with Pointers
```fortran
REAL, POINTER :: dynamic(:,:)
INTEGER :: m, n

! Allocate memory
ALLOCATE(dynamic(m,n))

! Use pointer
dynamic = 0.0

! Deallocate
DEALLOCATE(dynamic)

! Nullify pointer
NULLIFY(dynamic)
```

### Pointer Status
```fortran
! Check association
IF (ASSOCIATED(ptr)) THEN
  ! Pointer is associated
END IF

IF (ASSOCIATED(ptr, target)) THEN
  ! Pointer points to specific target
END IF

! Nullify pointer
NULLIFY(ptr)  ! Disassociate without deallocation
```

## Linked Lists

Common dynamic data structure using pointers.

```fortran
! Node definition
TYPE :: node
  INTEGER :: value
  TYPE(node), POINTER :: next => NULL()
END TYPE node

! List operations
TYPE(node), POINTER :: head, current, temp

! Create first node
ALLOCATE(head)
head%value = 1
NULLIFY(head%next)

! Add node
ALLOCATE(temp)
temp%value = 2
temp%next => head
head => temp

! Traverse list
current => head
DO WHILE (ASSOCIATED(current))
  PRINT *, current%value
  current => current%next
END DO

! Delete list
DO WHILE (ASSOCIATED(head))
  temp => head
  head => head%next
  DEALLOCATE(temp)
END DO
```

## Memory Management Best Practices

### 1. Always Deallocate
```fortran
SUBROUTINE process_data(n)
  INTEGER, INTENT(IN) :: n
  REAL, ALLOCATABLE :: work(:)
  
  ALLOCATE(work(n))
  ! Use work array
  
  ! Always clean up
  DEALLOCATE(work)
END SUBROUTINE
```

### 2. Check Allocation Status
```fortran
INTEGER :: alloc_stat
CHARACTER(LEN=100) :: error_msg

ALLOCATE(big_array(1000000), STAT=alloc_stat, ERRMSG=error_msg)
IF (alloc_stat /= 0) THEN
  PRINT *, "Allocation failed: ", TRIM(error_msg)
  STOP
END IF
```

### 3. Use MOVE_ALLOC for Efficiency
```fortran
REAL, ALLOCATABLE :: old_array(:), new_array(:)

! Resize array efficiently
ALLOCATE(new_array(new_size))
new_array(1:old_size) = old_array
CALL MOVE_ALLOC(new_array, old_array)
! new_array is now deallocated, old_array has new size
```

### 4. Automatic Deallocation
```fortran
! Allocatable arrays in procedures are automatically
! deallocated on exit (unless SAVE attribute)
SUBROUTINE auto_cleanup()
  REAL, ALLOCATABLE :: temp(:)
  ALLOCATE(temp(1000))
  ! No explicit DEALLOCATE needed
  ! temp is deallocated on return
END SUBROUTINE
```

## Common Patterns

### Dynamic 2D Array
```fortran
PROGRAM dynamic_matrix
  IMPLICIT NONE
  REAL, ALLOCATABLE :: matrix(:,:)
  INTEGER :: m, n, i, j
  
  PRINT *, "Enter dimensions:"
  READ *, m, n
  
  ALLOCATE(matrix(m,n))
  
  ! Initialize
  DO i = 1, m
    DO j = 1, n
      matrix(i,j) = i + j
    END DO
  END DO
  
  ! Use matrix
  PRINT *, "Sum:", SUM(matrix)
  
  DEALLOCATE(matrix)
END PROGRAM
```

### Growing Array
```fortran
MODULE growing_array_mod
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE append(array, value)
    REAL, ALLOCATABLE, INTENT(INOUT) :: array(:)
    REAL, INTENT(IN) :: value
    REAL, ALLOCATABLE :: temp(:)
    INTEGER :: n
    
    IF (ALLOCATED(array)) THEN
      n = SIZE(array)
      ALLOCATE(temp(n+1))
      temp(1:n) = array
      temp(n+1) = value
      CALL MOVE_ALLOC(temp, array)
    ELSE
      ALLOCATE(array(1))
      array(1) = value
    END IF
  END SUBROUTINE append
  
END MODULE
```

### Pointer to Array Section
```fortran
REAL, TARGET :: big_array(1000,1000)
REAL, POINTER :: window(:,:)

! Point to a section
window => big_array(100:200, 300:400)

! Modifications through window affect big_array
window = 0.0
```

## Advanced Topics

### Pointer Arrays vs Array Pointers
```fortran
! Array of pointers (each element is a pointer)
TYPE :: ptr_array
  REAL, POINTER :: ptr
END TYPE
TYPE(ptr_array) :: array_of_ptrs(10)

! Pointer to array (single pointer to whole array)
REAL, POINTER :: ptr_to_array(:)
```

### Memory Aliasing
```fortran
REAL, TARGET :: data(100)
REAL, POINTER :: p1(:), p2(:)

p1 => data(1:50)
p2 => data(26:75)
! p1 and p2 overlap - aliasing!
```

### SAVE Attribute
```fortran
SUBROUTINE remember_state()
  REAL, ALLOCATABLE, SAVE :: persistent(:)
  
  IF (.NOT. ALLOCATED(persistent)) THEN
    ALLOCATE(persistent(100))
    persistent = 0.0
  END IF
  
  ! persistent retains values between calls
END SUBROUTINE
```

## Memory Pitfalls to Avoid

1. **Memory Leaks**: Always deallocate allocated memory
2. **Dangling Pointers**: Don't use pointers after deallocation
3. **Aliasing Issues**: Be careful with overlapping pointers
4. **Stack Overflow**: Large automatic arrays can exceed stack size
5. **Uninitialized Pointers**: Always nullify or associate pointers

## Debugging Memory Issues

```fortran
! Use compiler flags:
! -check bounds     (Intel)
! -fbounds-check    (GNU)
! -Mbounds          (PGI)

! Track allocations
MODULE memory_tracker
  INTEGER :: total_allocated = 0
  
CONTAINS
  
  SUBROUTINE track_alloc(size)
    INTEGER, INTENT(IN) :: size
    total_allocated = total_allocated + size
    PRINT *, "Allocated:", size, "Total:", total_allocated
  END SUBROUTINE
  
END MODULE
```