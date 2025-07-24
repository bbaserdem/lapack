# Fortran Data Types

## Intrinsic Types

### INTEGER
```fortran
INTEGER :: i                    ! Default integer
INTEGER(KIND=1) :: small_int   ! Specific kind
INTEGER, PARAMETER :: ikind = SELECTED_INT_KIND(10)
INTEGER(KIND=ikind) :: big_int
```

### REAL
```fortran
REAL :: x                      ! Default precision
REAL(KIND=8) :: y             ! Double precision
DOUBLE PRECISION :: z         ! Traditional double

! Precision control
INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6, 37)    ! 6 decimal digits
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(15, 307)  ! 15 decimal digits
REAL(KIND=dp) :: high_precision
```

### COMPLEX
```fortran
COMPLEX :: c1                  ! Default complex
COMPLEX(KIND=8) :: c2         ! Double precision complex
c1 = (2.0, -1.0)              ! Literal complex
c2 = CMPLX(x, y, KIND=8)      ! Convert reals to complex
```

### LOGICAL
```fortran
LOGICAL :: flag = .TRUE.       ! Only .TRUE. or .FALSE.
LOGICAL(KIND=1) :: small_flag
```

### CHARACTER
```fortran
CHARACTER :: single_char       ! Single character (default)
CHARACTER(LEN=80) :: line     ! Fixed-length string
CHARACTER(LEN=*) :: assumed   ! Assumed length (dummy arguments)
CHARACTER(LEN=:), ALLOCATABLE :: varying  ! Varying length (F2003)
```

## Arrays

### Static Arrays
```fortran
! 1D arrays
INTEGER :: vector(10)                    ! Elements 1-10
REAL :: coords(3)                        ! Elements 1-3
INTEGER :: range(-10:10)                 ! Elements -10 to 10

! Multi-dimensional (up to 7 dimensions)
REAL :: matrix(3,3)                      ! 3x3 matrix
REAL :: tensor(10,20,30)                 ! 3D array
INTEGER :: bounds(0:9, -5:5, 1:100)      ! Custom bounds

! Array constants
INTEGER, PARAMETER :: primes(8) = (/ 2, 3, 5, 7, 11, 13, 17, 19 /)
```

### Dynamic Arrays
```fortran
! Allocatable arrays
REAL, ALLOCATABLE :: dynamic(:)          ! 1D allocatable
INTEGER, ALLOCATABLE :: grid(:,:)        ! 2D allocatable

! Allocation
ALLOCATE(dynamic(n))
ALLOCATE(grid(rows, cols), STAT=ierr)
DEALLOCATE(dynamic)

! Assumed shape (dummy arguments)
SUBROUTINE process(array)
  REAL, INTENT(IN) :: array(:,:)        ! Shape from actual argument
  INTEGER :: m, n
  m = SIZE(array, 1)                    ! First dimension size
  n = SIZE(array, 2)                    ! Second dimension size
END SUBROUTINE
```

### Array Properties
- **Rank**: Number of dimensions
- **Extent**: Size along each dimension
- **Shape**: Integer array of extents
- **Size**: Total number of elements
- **Conformable**: Arrays with same shape

### Array Operations
```fortran
! Whole array operations
A = B + C                      ! Element-wise addition
A = B * 2.0                    ! Scalar broadcast
A = SIN(B)                     ! Elemental function

! Array sections
A(1:5) = B(6:10)              ! Copy section
A(:,3) = 0.0                  ! Entire column
A(2,:) = vector               ! Entire row
A(1:10:2) = 1.0              ! Every other element (stride)

! Array constructors
vector = (/ 1.0, 2.0, 3.0 /)
vector = (/ (i*2.0, i=1,10) /)  ! Implied DO
```

## Derived Types

### Basic Definition
```fortran
TYPE :: person
  CHARACTER(LEN=30) :: name
  INTEGER :: age
  REAL :: height
END TYPE person

! Usage
TYPE(person) :: student
student%name = "John Doe"
student%age = 20
student%height = 1.75
```

### Complex Structures
```fortran
TYPE :: vector3d
  REAL :: x, y, z
END TYPE vector3d

TYPE :: particle
  TYPE(vector3d) :: position
  TYPE(vector3d) :: velocity
  REAL :: mass
  INTEGER :: id
END TYPE particle

TYPE :: node
  INTEGER :: value
  TYPE(node), POINTER :: next  ! Self-referential for linked lists
END TYPE node
```

### Arrays of Derived Types
```fortran
TYPE(particle) :: particles(1000)
TYPE(person), ALLOCATABLE :: population(:)

particles(1)%mass = 1.67e-27
ALLOCATE(population(n))
```

## Type Parameters and KIND

### KIND Parameter
- Controls precision/range of numeric types
- Processor-dependent values
- Use SELECTED_*_KIND for portability

### Functions for KIND
```fortran
! Get KIND for specific requirements
INTEGER, PARAMETER :: &
  int_kind = SELECTED_INT_KIND(10),        & ! 10 decimal digits
  real_kind = SELECTED_REAL_KIND(15, 307), & ! 15 digits, 10^307 range
  short = SELECTED_REAL_KIND(6)              ! 6 decimal digits

! Query KIND properties  
PRINT *, KIND(1.0)             ! Default real kind
PRINT *, PRECISION(1.0_dp)     ! Decimal precision
PRINT *, RANGE(1.0_dp)         ! Decimal exponent range
PRINT *, DIGITS(1)             ! Number of binary digits
PRINT *, EPSILON(1.0)          ! Machine epsilon
PRINT *, TINY(1.0)             ! Smallest positive number
PRINT *, HUGE(1.0)             ! Largest number
```

## Type Conversion

### Implicit Conversion
- Occurs in mixed-type expressions
- Hierarchy: COMPLEX > REAL > INTEGER
- Weaker type converts to stronger

### Explicit Conversion
```fortran
! Conversion functions
i = INT(3.7)                   ! Truncate to 3
i = NINT(3.7)                  ! Round to 4
r = REAL(i)                    ! Integer to real
r = REAL(i, KIND=dp)          ! Convert with specific kind
d = DBLE(r)                   ! To double precision
c = CMPLX(x, y)               ! Reals to complex
c = CMPLX(x, y, KIND=dp)      ! With specific kind
```

## Special Values
```fortran
! IEEE special values (when supported)
REAL :: inf, nan
inf = 1.0/0.0                  ! Infinity
nan = 0.0/0.0                  ! Not a Number

! Check for special values
IF (ISNAN(x)) THEN ...
IF (.NOT. ISFINITE(x)) THEN ...
```