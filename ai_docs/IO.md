# Fortran Input/Output

## List-Directed I/O (Free Format)

Simplest form, system determines format.

### Screen Output
```fortran
PRINT *, "Hello, World!"
PRINT *, "x =", x, "y =", y
PRINT *, array  ! Prints all elements
```

### Keyboard Input
```fortran
READ *, x, y, z  ! Reads three values
READ *, array    ! Reads array elements

! Input can be separated by:
! - spaces
! - commas
! - slashes (terminates input)
! - newlines
```

## Formatted I/O

Programmer controls exact format.

### Format Specifications

#### Basic Syntax
```fortran
PRINT fmt, variable_list
READ fmt, variable_list

! fmt can be:
! - Format string: "(format_descriptors)"
! - Statement label: 100
! - Character variable containing format
```

#### Common Edit Descriptors

**Integer (I)**
```fortran
Iw      ! Width w
Iw.m    ! Width w, minimum m digits

PRINT "(I5)", 42        ! "   42"
PRINT "(I5.3)", 42      ! "  042"
```

**Real (F, E, ES, EN)**
```fortran
Fw.d    ! Fixed: width w, d decimals
Ew.d    ! Exponential: width w, d decimals
ESw.d   ! Scientific: width w, d decimals
ENw.d   ! Engineering: width w, d decimals

PRINT "(F8.3)", 3.14159    ! "   3.142"
PRINT "(E10.3)", 3.14159   ! " 0.314E+01"
PRINT "(ES10.3)", 3.14159  ! " 3.142E+00"
```

**Character (A)**
```fortran
A       ! Default width
Aw      ! Width w

PRINT "(A)", "Hello"       ! "Hello"
PRINT "(A10)", "Hello"     ! "     Hello"
```

**Logical (L)**
```fortran
Lw      ! Width w

PRINT "(L1)", .TRUE.       ! "T"
PRINT "(L6)", .FALSE.      ! "     F"
```

**Other Descriptors**
```fortran
nX      ! n spaces
/       ! New line
Tc      ! Tab to column c
TLn     ! Tab left n positions
TRn     ! Tab right n positions
SP/SS   ! Sign control (plus/suppress)
```

### Format Examples
```fortran
! Multiple descriptors
PRINT "(A, I3, F8.2)", "Value:", n, x

! Repeat counts
PRINT "(3I5)", i, j, k               ! Three integers
PRINT "(5(F6.2, 1X))", array(1:5)    ! Five reals with spaces

! Complex formatting
WRITE (*, "(A, /, 3(I5, 2X, F8.3, /))")  &
      "Results:", (i, results(i), i=1,3)
```

## File I/O

### Opening Files
```fortran
OPEN(UNIT=unit_number, FILE=filename, STATUS=status, &
     ACTION=action, IOSTAT=ios)

! Parameters:
! UNIT: positive integer (avoid 0, 5, 6)
! FILE: filename string
! STATUS: 'NEW', 'OLD', 'REPLACE', 'SCRATCH', 'UNKNOWN'
! ACTION: 'READ', 'WRITE', 'READWRITE'
! IOSTAT: error code (0 = success)

! Example
INTEGER :: ios
OPEN(UNIT=10, FILE="data.txt", STATUS="OLD", &
     ACTION="READ", IOSTAT=ios)
IF (ios /= 0) THEN
  PRINT *, "Error opening file"
  STOP
END IF
```

### Writing to Files
```fortran
! List-directed
WRITE(10, *) x, y, z

! Formatted
WRITE(10, "(3F10.4)") x, y, z

! With error checking
WRITE(10, "(A)", IOSTAT=ios) line
IF (ios /= 0) PRINT *, "Write error"
```

### Reading from Files
```fortran
! List-directed
READ(10, *, IOSTAT=ios) x, y, z

! Formatted
READ(10, "(3F10.4)", IOSTAT=ios) x, y, z

! Check for end-of-file
IF (ios < 0) THEN
  PRINT *, "End of file reached"
ELSE IF (ios > 0) THEN
  PRINT *, "Read error"
END IF
```

### Closing Files
```fortran
CLOSE(UNIT=10, STATUS=status, IOSTAT=ios)

! STATUS options:
! 'KEEP' - file remains (default)
! 'DELETE' - file is deleted
```

## Advanced File Operations

### File Positioning
```fortran
REWIND(10)              ! Go to beginning
BACKSPACE(10)           ! Back one record
ENDFILE(10)             ! Write end-of-file

! Direct access positioning
READ(10, REC=n) data    ! Read record n
WRITE(10, REC=n) data   ! Write record n
```

### File Inquiry
```fortran
LOGICAL :: exists, opened
INTEGER :: reclen
CHARACTER(LEN=20) :: fname

INQUIRE(FILE="data.txt", EXIST=exists)
INQUIRE(UNIT=10, OPENED=opened, NAME=fname)
INQUIRE(IOLENGTH=reclen) real_array  ! Record length
```

### Direct Access Files
```fortran
! Fixed-length records
OPEN(UNIT=20, FILE="direct.dat", STATUS="NEW", &
     ACCESS="DIRECT", RECL=100)

! Write record 5
WRITE(20, REC=5) data

! Read record 3
READ(20, REC=3) data
```

### Stream Access (F2003)
```fortran
OPEN(UNIT=30, FILE="stream.dat", ACCESS="STREAM")

! Position and read
READ(30, POS=100) byte_data
```

## Internal Files

Character variables used for I/O operations.

```fortran
CHARACTER(LEN=80) :: buffer
REAL :: x = 3.14159

! Write to string
WRITE(buffer, "(F10.4)") x

! Read from string
READ(buffer, *) x

! Useful for conversions
CHARACTER(LEN=20) :: number_string
INTEGER :: number
number_string = "12345"
READ(number_string, *) number
```

## Namelist I/O

Groups variables for convenient I/O.

```fortran
! Define namelist
INTEGER :: n
REAL :: x, y
CHARACTER(LEN=20) :: name
NAMELIST /params/ n, x, y, name

! Write namelist
OPEN(10, FILE="params.nml")
WRITE(10, NML=params)
CLOSE(10)

! Output format:
! &PARAMS
!  N=10,
!  X=1.5,
!  Y=2.5,
!  NAME="test"
! /

! Read namelist
OPEN(10, FILE="params.nml")
READ(10, NML=params)
CLOSE(10)
```

## Common I/O Patterns

### Reading Until EOF
```fortran
INTEGER :: ios
REAL :: value

OPEN(10, FILE="data.txt", STATUS="OLD")
DO
  READ(10, *, IOSTAT=ios) value
  IF (ios /= 0) EXIT
  ! Process value
END DO
CLOSE(10)
```

### Error Handling
```fortran
INTEGER :: ios
CHARACTER(LEN=100) :: error_msg

OPEN(10, FILE="data.txt", IOSTAT=ios, IOMSG=error_msg)
IF (ios /= 0) THEN
  PRINT *, "Error: ", TRIM(error_msg)
  STOP
END IF
```

### CSV File Reading
```fortran
CHARACTER(LEN=200) :: line
REAL :: x, y, z
INTEGER :: ios

OPEN(10, FILE="data.csv")
READ(10, *)  ! Skip header
DO
  READ(10, "(A)", IOSTAT=ios) line
  IF (ios /= 0) EXIT
  READ(line, *) x, y, z
  ! Process x, y, z
END DO
CLOSE(10)
```

### Binary File I/O
```fortran
! Unformatted (binary) I/O
OPEN(20, FILE="binary.dat", FORM="UNFORMATTED", &
     ACCESS="SEQUENTIAL")

! Write binary
WRITE(20) n, array

! Read binary
READ(20) n, array

CLOSE(20)
```

## Best Practices

1. **Always check IOSTAT** for error handling
2. **Use meaningful unit numbers** (avoid 0, 5, 6)
3. **Close files** when done
4. **Use formatted I/O** for human-readable files
5. **Use unformatted I/O** for efficiency with large data
6. **Document format strings** for complex formats
7. **Use INQUIRE** to check file status before operations