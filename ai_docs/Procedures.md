# Fortran Procedures and Modules

## Functions

Functions return a single value and should not modify their arguments.

### Basic Function
```fortran
FUNCTION function_name(arg1, arg2) RESULT(return_var)
  IMPLICIT NONE
  ! Argument declarations with INTENT
  REAL, INTENT(IN) :: arg1, arg2
  REAL :: return_var
  
  ! Function body
  return_var = arg1 + arg2
END FUNCTION function_name

! Alternative syntax (function name as result)
REAL FUNCTION add(x, y)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x, y
  add = x + y
END FUNCTION add
```

### Recursive Function
```fortran
RECURSIVE FUNCTION factorial(n) RESULT(fact)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: fact
  
  IF (n <= 1) THEN
    fact = 1
  ELSE
    fact = n * factorial(n-1)
  END IF
END FUNCTION factorial
```

### Elemental Function
Works on scalars and arrays element-by-element.

```fortran
ELEMENTAL FUNCTION degrees_to_radians(degrees) RESULT(radians)
  IMPLICIT NONE
  REAL, INTENT(IN) :: degrees
  REAL :: radians
  REAL, PARAMETER :: pi = 3.14159265359
  
  radians = degrees * pi / 180.0
END FUNCTION degrees_to_radians
```

### Pure Function
No side effects, same input always gives same output.

```fortran
PURE FUNCTION distance(x1, y1, x2, y2) RESULT(dist)
  IMPLICIT NONE
  REAL, INTENT(IN) :: x1, y1, x2, y2
  REAL :: dist
  
  dist = SQRT((x2-x1)**2 + (y2-y1)**2)
END FUNCTION distance
```

## Subroutines

Subroutines perform actions and can modify their arguments.

### Basic Subroutine
```fortran
SUBROUTINE swap(a, b)
  IMPLICIT NONE
  REAL, INTENT(INOUT) :: a, b
  REAL :: temp
  
  temp = a
  a = b
  b = temp
END SUBROUTINE swap

! Call with CALL statement
CALL swap(x, y)
```

### Array Arguments
```fortran
SUBROUTINE process_array(arr, n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(INOUT) :: arr(n)  ! Explicit shape
  
  arr = arr * 2.0
END SUBROUTINE process_array

! Assumed shape (requires interface)
SUBROUTINE process_assumed(arr)
  IMPLICIT NONE
  REAL, INTENT(INOUT) :: arr(:)  ! Assumed shape
  
  arr = arr * 2.0
END SUBROUTINE process_assumed
```

### Optional Arguments
```fortran
SUBROUTINE print_info(name, age, city)
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: name
  INTEGER, INTENT(IN), OPTIONAL :: age
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: city
  
  PRINT *, "Name: ", name
  IF (PRESENT(age)) PRINT *, "Age: ", age
  IF (PRESENT(city)) PRINT *, "City: ", city
END SUBROUTINE print_info

! Calls
CALL print_info("John")
CALL print_info("Jane", age=25)
CALL print_info("Bob", city="NYC", age=30)
```

## INTENT Attribute

Declares how dummy arguments are used.

```fortran
SUBROUTINE example(input, output, both)
  IMPLICIT NONE
  REAL, INTENT(IN) :: input      ! Read-only
  REAL, INTENT(OUT) :: output    ! Write-only
  REAL, INTENT(INOUT) :: both    ! Read and write
  
  output = input * 2.0
  both = both + input
END SUBROUTINE example
```

## Interfaces

Provide explicit interfaces for external procedures.

### Interface Block
```fortran
INTERFACE
  SUBROUTINE external_sub(x, y, z)
    REAL, INTENT(IN) :: x, y
    REAL, INTENT(OUT) :: z
  END SUBROUTINE external_sub
  
  REAL FUNCTION external_func(a, b)
    REAL, INTENT(IN) :: a, b
  END FUNCTION external_func
END INTERFACE
```

### Generic Interfaces
```fortran
INTERFACE swap
  MODULE PROCEDURE swap_real, swap_int, swap_complex
END INTERFACE swap

! Usage
CALL swap(real1, real2)     ! Calls swap_real
CALL swap(int1, int2)       ! Calls swap_int
```

### Operator Overloading
```fortran
MODULE vector_ops
  TYPE :: vector
    REAL :: x, y, z
  END TYPE vector
  
  INTERFACE OPERATOR(+)
    MODULE PROCEDURE add_vectors
  END INTERFACE
  
CONTAINS
  FUNCTION add_vectors(v1, v2) RESULT(v3)
    TYPE(vector), INTENT(IN) :: v1, v2
    TYPE(vector) :: v3
    v3%x = v1%x + v2%x
    v3%y = v1%y + v2%y
    v3%z = v1%z + v2%z
  END FUNCTION add_vectors
END MODULE vector_ops
```

## Modules

Package related data and procedures together.

### Basic Module
```fortran
MODULE constants
  IMPLICIT NONE
  REAL, PARAMETER :: pi = 3.14159265359
  REAL, PARAMETER :: e = 2.71828182846
  REAL, PARAMETER :: c = 299792458.0  ! Speed of light
END MODULE constants

! Usage
PROGRAM main
  USE constants
  IMPLICIT NONE
  PRINT *, "Pi =", pi
END PROGRAM main
```

### Module with Procedures
```fortran
MODULE math_tools
  IMPLICIT NONE
  PRIVATE  ! Default accessibility
  PUBLIC :: mean, stdev  ! Explicitly public
  
CONTAINS
  
  REAL FUNCTION mean(array)
    REAL, INTENT(IN) :: array(:)
    mean = SUM(array) / SIZE(array)
  END FUNCTION mean
  
  REAL FUNCTION stdev(array)
    REAL, INTENT(IN) :: array(:)
    REAL :: avg
    avg = mean(array)
    stdev = SQRT(SUM((array - avg)**2) / (SIZE(array) - 1))
  END FUNCTION stdev
  
END MODULE math_tools
```

### Module with Derived Types
```fortran
MODULE particle_module
  IMPLICIT NONE
  
  TYPE :: particle
    PRIVATE  ! Data hiding
    REAL :: x, y, z
    REAL :: mass
  END TYPE particle
  
  ! Constructor interface
  INTERFACE particle
    MODULE PROCEDURE create_particle
  END INTERFACE
  
CONTAINS
  
  TYPE(particle) FUNCTION create_particle(x, y, z, m)
    REAL, INTENT(IN) :: x, y, z, m
    create_particle%x = x
    create_particle%y = y
    create_particle%z = z
    create_particle%mass = m
  END FUNCTION create_particle
  
  REAL FUNCTION get_mass(p)
    TYPE(particle), INTENT(IN) :: p
    get_mass = p%mass
  END FUNCTION get_mass
  
END MODULE particle_module
```

## Internal Procedures

Procedures contained within other program units.

```fortran
PROGRAM main
  IMPLICIT NONE
  REAL :: x = 5.0
  
  CALL process()
  
CONTAINS
  
  SUBROUTINE process()
    ! Can access host variables
    x = x * 2.0
    PRINT *, "x =", x
  END SUBROUTINE process
  
END PROGRAM main
```

## Procedure Arguments

### Passing Procedures
```fortran
PROGRAM main
  IMPLICIT NONE
  REAL :: result
  
  result = integrate(my_func, 0.0, 1.0)
  
CONTAINS
  
  REAL FUNCTION my_func(x)
    REAL, INTENT(IN) :: x
    my_func = x**2
  END FUNCTION my_func
  
  REAL FUNCTION integrate(f, a, b)
    INTERFACE
      REAL FUNCTION f(x)
        REAL, INTENT(IN) :: x
      END FUNCTION f
    END INTERFACE
    REAL, INTENT(IN) :: a, b
    ! Integration implementation
  END FUNCTION integrate
  
END PROGRAM main
```

## USE Statement Options

### Selective Import
```fortran
USE module_name, ONLY: item1, item2
```

### Renaming
```fortran
USE module_name, local_name => module_name
USE constants, mypi => pi
```

## Best Practices

### 1. Always Use IMPLICIT NONE
```fortran
MODULE good_module
  IMPLICIT NONE  ! At module level
  ! ...
END MODULE
```

### 2. Declare INTENT for All Arguments
```fortran
SUBROUTINE process(input, output)
  REAL, INTENT(IN) :: input
  REAL, INTENT(OUT) :: output
  ! Clear intention
END SUBROUTINE
```

### 3. Use Modules for Related Code
```fortran
MODULE linear_algebra
  ! Types, constants, procedures all related
  ! to linear algebra operations
END MODULE
```

### 4. Control Accessibility
```fortran
MODULE implementation
  IMPLICIT NONE
  PRIVATE  ! Hide implementation details
  PUBLIC :: api_function  ! Expose only interface
  ! ...
END MODULE
```

### 5. Use Assumed Shape Arrays
```fortran
! Requires interface but more flexible
SUBROUTINE process(array)
  REAL, INTENT(INOUT) :: array(:,:)
  ! Works with any 2D array
END SUBROUTINE
```