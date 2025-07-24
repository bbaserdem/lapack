# Fortran Intrinsic Functions

## Numeric Functions

### Arithmetic
```fortran
ABS(x)              ! Absolute value
MOD(a, p)           ! Remainder: a - INT(a/p)*p
MODULO(a, p)        ! Modulo: a - FLOOR(a/p)*p
SIGN(a, b)          ! |a| with sign of b
DIM(x, y)           ! MAX(x-y, 0)
MAX(a1, a2, ...)    ! Maximum value
MIN(a1, a2, ...)    ! Minimum value
```

### Rounding and Truncation
```fortran
AINT(x)             ! Truncate to whole number (keeps type)
ANINT(x)            ! Round to nearest whole (keeps type)
CEILING(x)          ! Smallest integer >= x
FLOOR(x)            ! Largest integer <= x
INT(x)              ! Convert to integer (truncate toward zero)
NINT(x)             ! Convert to nearest integer
```

### Mathematical
```fortran
SQRT(x)             ! Square root
EXP(x)              ! e^x
LOG(x)              ! Natural logarithm (ln)
LOG10(x)            ! Base-10 logarithm
```

### Trigonometric (angles in radians)
```fortran
SIN(x)              ! Sine
COS(x)              ! Cosine
TAN(x)              ! Tangent
ASIN(x)             ! Arcsine (-1 <= x <= 1)
ACOS(x)             ! Arccosine (-1 <= x <= 1)
ATAN(x)             ! Arctangent
ATAN2(y, x)         ! Arctangent of y/x (correct quadrant)
```

### Hyperbolic
```fortran
SINH(x)             ! Hyperbolic sine
COSH(x)             ! Hyperbolic cosine
TANH(x)             ! Hyperbolic tangent
ASINH(x)            ! Inverse hyperbolic sine (F2008)
ACOSH(x)            ! Inverse hyperbolic cosine (F2008)
ATANH(x)            ! Inverse hyperbolic tangent (F2008)
```

## Complex Number Functions
```fortran
AIMAG(z)            ! Imaginary part
REAL(z)             ! Real part (also type conversion)
CONJG(z)            ! Complex conjugate
ABS(z)              ! Magnitude |z|
CMPLX(x, y, kind)   ! Construct complex number
```

## Type Conversion
```fortran
REAL(x, kind)       ! Convert to real
INT(x, kind)        ! Convert to integer (truncate)
NINT(x, kind)       ! Convert to nearest integer
CMPLX(x, y, kind)   ! Convert to complex
DBLE(x)             ! Convert to double precision
LOGICAL(x, kind)    ! Convert to logical
```

## Character Functions
```fortran
LEN(string)         ! Length of string
LEN_TRIM(string)    ! Length without trailing blanks
TRIM(string)        ! Remove trailing blanks
ADJUSTL(string)     ! Left justify
ADJUSTR(string)     ! Right justify
INDEX(string, sub)  ! Position of substring (0 if not found)
SCAN(string, set)   ! Position of first char in set
VERIFY(str, set)    ! Position of first char NOT in set
REPEAT(string, n)   ! Repeat string n times

! Character/Integer conversion
CHAR(i, kind)       ! Integer to character
ICHAR(c)            ! Character to integer (ASCII value)
IACHAR(c)           ! Character to ASCII value
ACHAR(i)            ! ASCII value to character
```

## Array Functions

### Array Properties
```fortran
SIZE(array, dim)    ! Number of elements (along dimension)
SHAPE(array)        ! Integer array of dimensions
LBOUND(array, dim)  ! Lower bounds
UBOUND(array, dim)  ! Upper bounds
ALLOCATED(array)    ! True if allocated
```

### Array Manipulation
```fortran
RESHAPE(source, shape)           ! Reshape array
TRANSPOSE(matrix)                ! Matrix transpose
SPREAD(source, dim, n)          ! Replicate array
PACK(array, mask, vector)       ! Pack array under mask
UNPACK(vector, mask, field)     ! Unpack under mask
MERGE(tsource, fsource, mask)   ! Merge arrays by mask
CSHIFT(array, shift, dim)       ! Circular shift
EOSHIFT(array, shift, bound, dim) ! End-off shift
```

### Array Reduction
```fortran
ALL(mask, dim)      ! True if all elements true
ANY(mask, dim)      ! True if any element true
COUNT(mask, dim)    ! Count true elements
MAXVAL(array, mask) ! Maximum value
MINVAL(array, mask) ! Minimum value
PRODUCT(array, mask)! Product of elements
SUM(array, mask)    ! Sum of elements
```

### Array Location
```fortran
MAXLOC(array, mask) ! Location of maximum
MINLOC(array, mask) ! Location of minimum
FINDLOC(array, val) ! Location of value (F2008)
```

### Array Mathematics
```fortran
DOT_PRODUCT(a, b)   ! Dot product of vectors
MATMUL(a, b)        ! Matrix multiplication
```

## Bit Manipulation
```fortran
IAND(i, j)          ! Bitwise AND
IEOR(i, j)          ! Bitwise exclusive OR
IOR(i, j)           ! Bitwise OR
NOT(i)              ! Bitwise NOT
BTEST(i, pos)       ! Test bit at position
IBCLR(i, pos)       ! Clear bit
IBSET(i, pos)       ! Set bit
IBITS(i, pos, len)  ! Extract bit field
ISHFT(i, shift)     ! Logical shift
ISHFTC(i, shift)    ! Circular shift
MVBITS(from, fpos, len, to, tpos) ! Move bits
```

## Inquiry Functions

### Numeric Inquiry
```fortran
KIND(x)             ! KIND parameter value
DIGITS(x)           ! Number of significant digits
EPSILON(x)          ! Machine epsilon
HUGE(x)             ! Largest number
MAXEXPONENT(x)      ! Maximum exponent
MINEXPONENT(x)      ! Minimum exponent
PRECISION(x)        ! Decimal precision
RADIX(x)            ! Base of number system
RANGE(x)            ! Decimal exponent range
TINY(x)             ! Smallest positive number
```

### KIND Selection
```fortran
SELECTED_INT_KIND(r)        ! Integer kind for range 10^r
SELECTED_REAL_KIND(p, r)    ! Real kind for precision p, range r
SELECTED_CHAR_KIND(name)    ! Character kind (F2003)
```

## Special Functions

### Pointer/Allocation Status
```fortran
ALLOCATED(array)    ! True if allocated
ASSOCIATED(ptr)     ! True if pointer associated
ASSOCIATED(ptr, tgt)! True if ptr points to tgt
NULL(mold)          ! Null pointer
LOC(x)              ! Address of x (non-standard)
```

### Argument Presence
```fortran
PRESENT(arg)        ! True if optional argument present
```

### Random Numbers
```fortran
CALL RANDOM_NUMBER(x)    ! Uniform random [0,1)
CALL RANDOM_SEED(size, put, get) ! Control generator
```

### System
```fortran
CALL SYSTEM_CLOCK(count, rate, max) ! System clock
CALL DATE_AND_TIME(date, time, zone, values) ! Date/time
CALL CPU_TIME(time)      ! CPU time in seconds
COMMAND_ARGUMENT_COUNT() ! Number of command arguments
CALL GET_COMMAND_ARGUMENT(n, value, length, status)
CALL GET_ENVIRONMENT_VARIABLE(name, value, length, status)
```

## Usage Examples

```fortran
! Mathematical calculations
y = SQRT(x**2 + z**2)
angle = ATAN2(y, x) * 180.0/pi

! Array operations
mean = SUM(data) / SIZE(data)
stdev = SQRT(SUM((data - mean)**2) / (SIZE(data) - 1))

! String manipulation
clean_string = TRIM(ADJUSTL(input))
pos = INDEX(filename, '.f90')

! Type conversions with precision
REAL(KIND=dp) :: x
x = REAL(int_value, KIND=dp)

! Bit operations
IF (BTEST(flags, 3)) THEN  ! Test bit 3
  flags = IBCLR(flags, 3)   ! Clear bit 3
END IF
```