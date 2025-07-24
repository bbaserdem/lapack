# Fortran Control Flow

## IF Constructs

### Simple IF
```fortran
IF (logical_expression) statement

IF (x < 0) x = -x  ! One-line IF
```

### Block IF
```fortran
IF (condition) THEN
  ! statements
END IF

! Named IF (for clarity in nested constructs)
outer: IF (x > 0) THEN
  inner: IF (y > 0) THEN
    ! statements
  END IF inner
END IF outer
```

### IF-ELSE
```fortran
IF (condition) THEN
  ! true branch
ELSE
  ! false branch
END IF
```

### IF-ELSE IF-ELSE
```fortran
IF (score >= 90) THEN
  grade = 'A'
ELSE IF (score >= 80) THEN
  grade = 'B'
ELSE IF (score >= 70) THEN
  grade = 'C'
ELSE
  grade = 'F'
END IF
```

## SELECT CASE

For multi-way branching based on value of expression (integer, logical, or character only - NOT real).

```fortran
SELECT CASE (expression)
CASE (value1)
  ! statements
CASE (value2, value3)  ! Multiple values
  ! statements
CASE (low:high)        ! Range
  ! statements
CASE (:5)              ! Up to 5
  ! statements
CASE (10:)             ! 10 and above
  ! statements
CASE DEFAULT           ! Optional default
  ! statements
END SELECT

! Example
SELECT CASE (day)
CASE (1)
  dayname = "Monday"
CASE (2:5)
  dayname = "Weekday"
CASE (6,7)
  dayname = "Weekend"
CASE DEFAULT
  dayname = "Invalid"
END SELECT

! Named SELECT CASE
check: SELECT CASE (ch)
CASE ('a':'z') check
  PRINT *, "Lowercase"
CASE ('A':'Z') check
  PRINT *, "Uppercase"
CASE DEFAULT check
  PRINT *, "Not a letter"
END SELECT check
```

## DO Loops

### Counter-Controlled DO
```fortran
DO i = start, end [, step]
  ! statements
END DO

! Examples
DO i = 1, 10           ! 1 to 10, step 1
  PRINT *, i
END DO

DO i = 10, 1, -1       ! 10 to 1, step -1
  PRINT *, i
END DO

DO i = 0, 100, 5       ! 0 to 100, step 5
  PRINT *, i
END DO
```

### Named DO Loops
```fortran
outer: DO i = 1, n
  inner: DO j = 1, m
    IF (condition) EXIT outer    ! Exit outer loop
    IF (condition) CYCLE inner   ! Next iteration of inner
  END DO inner
END DO outer
```

### DO WHILE
```fortran
DO WHILE (condition)
  ! statements
END DO

! Example
x = 1.0
DO WHILE (x < 1000.0)
  x = x * 2.0
END DO
```

### Endless DO
```fortran
DO
  ! statements
  IF (condition) EXIT
  ! more statements
END DO

! Named endless loop
main: DO
  READ (*,*, IOSTAT=ios) x
  IF (ios /= 0) EXIT main
  ! process x
END DO main
```

## Loop Control

### EXIT
Terminates loop and continues after END DO.

```fortran
DO i = 1, n
  IF (array(i) == target) THEN
    found_index = i
    EXIT
  END IF
END DO

! EXIT from nested loops
outer: DO i = 1, n
  inner: DO j = 1, m
    IF (condition) EXIT outer  ! Exit both loops
  END DO inner
END DO outer
```

### CYCLE
Skips rest of current iteration, continues with next.

```fortran
DO i = 1, n
  IF (array(i) < 0) CYCLE  ! Skip negative values
  sum = sum + array(i)
END DO
```

## Array Control Constructs

### WHERE
Conditional array assignment.

```fortran
WHERE (mask)
  ! assignments where mask is true
END WHERE

WHERE (mask)
  ! assignments where mask is true
ELSEWHERE
  ! assignments where mask is false
END WHERE

! Examples
WHERE (array > 0)
  array = SQRT(array)
END WHERE

WHERE (A > 0.0)
  B = LOG(A)
ELSEWHERE
  B = 0.0
END WHERE

! Multiple statements
WHERE (matrix /= 0.0)
  inverse = 1.0 / matrix
  flag = .TRUE.
ELSEWHERE
  inverse = 0.0
  flag = .FALSE.
END WHERE
```

### FORALL (obsolescent in F2018)
Parallel array assignments.

```fortran
FORALL (i = 1:n, j = 1:m, i <= j)
  upper(i,j) = matrix(i,j)
END FORALL

! With mask
FORALL (i = 1:n, array(i) /= 0.0)
  reciprocal(i) = 1.0 / array(i)
END FORALL
```

## GOTO (Discouraged)

Unconditional jump to labeled statement.

```fortran
GOTO 100
! skipped code
100 CONTINUE  ! Label

! Computed GOTO (obsolete)
! Assigned GOTO (obsolete)
```

## STOP

Terminates program execution.

```fortran
STOP           ! Normal termination
STOP 'Error'   ! With message
STOP 999       ! With numeric code
```

## Best Practices

### 1. Use Block Constructs
```fortran
! Good - clear structure
IF (x > 0) THEN
  y = SQRT(x)
ELSE
  y = 0.0
END IF

! Avoid - single line for complex logic
IF (x > 0) y = SQRT(x)
```

### 2. Name Complex Nested Constructs
```fortran
row_loop: DO i = 1, n
  col_loop: DO j = 1, m
    IF (matrix(i,j) == target) THEN
      PRINT *, "Found at", i, j
      EXIT row_loop
    END IF
  END DO col_loop
END DO row_loop
```

### 3. Avoid GOTO
Use structured constructs instead:
```fortran
! Instead of GOTO
found = .FALSE.
search: DO i = 1, n
  IF (array(i) == target) THEN
    found = .TRUE.
    EXIT search
  END IF
END DO search
```

### 4. Use WHERE for Array Operations
```fortran
! Instead of loops
WHERE (temperatures < 0.0)
  state = 'solid'
ELSEWHERE (temperatures < 100.0)
  state = 'liquid'
ELSEWHERE
  state = 'gas'
END WHERE
```

## Common Patterns

### Search Pattern
```fortran
found = .FALSE.
DO i = 1, SIZE(array)
  IF (array(i) == target) THEN
    found = .TRUE.
    location = i
    EXIT
  END IF
END DO
```

### Accumulation Pattern
```fortran
sum = 0.0
DO i = 1, n
  sum = sum + array(i)
END DO
```

### Nested Loop with Early Exit
```fortran
outer: DO i = 1, n
  inner: DO j = 1, m
    result = process(i, j)
    IF (result < tolerance) EXIT outer
  END DO inner
END DO outer
```