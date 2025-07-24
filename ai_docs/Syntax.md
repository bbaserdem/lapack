# Fortran Syntax Guide

## Basic Elements

### Character Set
- **Letters**: A-Z, a-z (case-insensitive)
- **Digits**: 0-9
- **Special Characters**: = : + blank - * / ( ) , . $ ' ! " % & ; ? _

### Identifiers
- Maximum 31 characters
- Start with letter, followed by letters, digits, or underscores
- Case-insensitive (PRINT = print)

### Statement Rules
- Maximum 132 characters per line
- Continuation with `&` at end of line (max 39 continuation lines)
- Comments start with `!`
- Free-form source code

## Program Structure

```fortran
PROGRAM program_name
  IMPLICIT NONE    ! Always use to disable implicit typing
  ! declarations
  ! executable statements
END PROGRAM program_name
```

### Other Program Units
```fortran
MODULE module_name
  ! specifications
CONTAINS
  ! module procedures
END MODULE module_name

FUNCTION function_name(args)
  ! body
END FUNCTION function_name

SUBROUTINE subroutine_name(args)
  ! body
END SUBROUTINE subroutine_name
```

## Operators

### Arithmetic Operators (precedence high to low)
1. `**` (exponentiation, right-to-left)
2. `*` `/` (multiplication, division)
3. `+` `-` (addition, subtraction)

### Relational Operators
- `<` `<=` `>` `>=` `==` `/=`
- `.LT.` `.LE.` `.GT.` `.GE.` `.EQ.` `.NE.` (alternative forms)

### Logical Operators (precedence high to low)
1. `.NOT.`
2. `.AND.`
3. `.OR.`
4. `.EQV.` `.NEQV.`

### Character Operator
- `//` (concatenation)

## Expressions

### Numeric Expressions
- Integer division truncates toward zero: `3/2 = 1`, `-3/2 = -1`
- Mixed-type expressions convert to stronger type: complex > real > integer
- Use `REAL()` to avoid integer division: `REAL(2)/3 = 0.666...`

### Logical Expressions
- Result in `.TRUE.` or `.FALSE.`
- Careful with real comparisons: use `ABS(a-b) < delta` instead of `a == b`

### Array Expressions
- Element-by-element operations on conformable arrays
- Scalar operations broadcast to all elements: `array * 2.0`

## Declaration Syntax

### Variables
```fortran
type_spec [,attributes] :: variable_list

INTEGER :: i, j
REAL :: x, y
REAL(KIND=8) :: high_precision
CHARACTER(LEN=80) :: line
LOGICAL :: flag
```

### Constants
```fortran
type_spec, PARAMETER :: name = value

REAL, PARAMETER :: pi = 3.14159
INTEGER, PARAMETER :: max_size = 1000
```

### Common Attributes
- `PARAMETER` - named constant
- `DIMENSION(shape)` - array specification
- `ALLOCATABLE` - dynamic allocation
- `POINTER` - pointer variable
- `TARGET` - can be pointed to
- `INTENT(IN/OUT/INOUT)` - dummy argument intent
- `PUBLIC`/`PRIVATE` - module accessibility
- `SAVE` - preserve value between calls

## Statement Keywords

### Declarations
`INTEGER`, `REAL`, `DOUBLE PRECISION`, `COMPLEX`, `LOGICAL`, `CHARACTER`, `TYPE`

### Control Flow
`IF`, `THEN`, `ELSE`, `ELSE IF`, `END IF`, `SELECT CASE`, `CASE`, `CASE DEFAULT`, `END SELECT`, `DO`, `DO WHILE`, `END DO`, `CYCLE`, `EXIT`, `GOTO`, `WHERE`, `FORALL`

### Program Units
`PROGRAM`, `MODULE`, `FUNCTION`, `SUBROUTINE`, `CONTAINS`, `END`, `INTERFACE`, `END INTERFACE`, `USE`, `RECURSIVE`

### I/O
`PRINT`, `READ`, `WRITE`, `OPEN`, `CLOSE`, `INQUIRE`

### Memory
`ALLOCATE`, `DEALLOCATE`, `NULLIFY`, `ASSOCIATED`

### Execution Control
`CALL`, `RETURN`, `STOP`

## Obsolescent Features (avoid)
- `COMMON` blocks
- `EQUIVALENCE`
- `GOTO` (discouraged but not obsolete)
- `ENTRY`, `PAUSE`, `ASSIGN`
- `BLOCK DATA`
- Fixed-form source
- Implicit typing (without IMPLICIT NONE)