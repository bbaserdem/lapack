!> \brief \b LSAME tests two characters for equality regardless of case.
!>
!> \par Purpose:
!> =============
!>
!> LSAME returns .TRUE. if CA is the same letter as CB regardless of case.
!>
!> \param[in] CA
!>          CA is CHARACTER*1
!>
!> \param[in] CB  
!>          CB is CHARACTER*1
!>          CA and CB specify the single characters to be compared.

LOGICAL FUNCTION LSAME(CA, CB)
    IMPLICIT NONE
    
    ! Arguments
    CHARACTER, INTENT(IN) :: CA, CB
    
    ! Local variables
    INTEGER :: INTA, INTB, ZCODE
    
    ! Intrinsic functions
    INTRINSIC :: ICHAR
    
    ! Test if the characters are equal
    LSAME = CA == CB
    IF (LSAME) RETURN
    
    ! Now test for equivalence if both characters are alphabetic
    ZCODE = ICHAR('Z')
    
    ! Use 'Z' rather than 'A' so that ASCII can be detected on Prime
    ! machines, on which ICHAR returns a value with bit 8 set.
    ! ICHAR('A') on Prime machines returns 193 which is the same as
    ! ICHAR('A') on an EBCDIC machine.
    
    INTA = ICHAR(CA)
    INTB = ICHAR(CB)
    
    IF (ZCODE == 90 .OR. ZCODE == 122) THEN
        ! ASCII is assumed - ZCODE is the ASCII code of either lower or
        ! upper case 'Z'.
        IF (INTA >= 97 .AND. INTA <= 122) INTA = INTA - 32
        IF (INTB >= 97 .AND. INTB <= 122) INTB = INTB - 32
    ELSE IF (ZCODE == 233 .OR. ZCODE == 169) THEN
        ! EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
        ! upper case 'Z'.
        IF (INTA >= 129 .AND. INTA <= 137 .OR. &
            INTA >= 145 .AND. INTA <= 153 .OR. &
            INTA >= 162 .AND. INTA <= 169) INTA = INTA + 64
        IF (INTB >= 129 .AND. INTB <= 137 .OR. &
            INTB >= 145 .AND. INTB <= 153 .OR. &
            INTB >= 162 .AND. INTB <= 169) INTB = INTB + 64
    ELSE IF (ZCODE == 218 .OR. ZCODE == 250) THEN
        ! ASCII is assumed, on Prime machines - ZCODE is the ASCII code
        ! plus 128 of either lower or upper case 'Z'.
        IF (INTA >= 225 .AND. INTA <= 250) INTA = INTA - 32
        IF (INTB >= 225 .AND. INTB <= 250) INTB = INTB - 32
    END IF
    
    LSAME = INTA == INTB
    
    RETURN
END FUNCTION LSAME