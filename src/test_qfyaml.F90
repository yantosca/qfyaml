!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test_QFYAML
!
! !DESCRIPTION: Test program for developing QFYAML features.
!\\
!\\
! !INTERFACE:
!
PROGRAM Test_QFYAML
!
! !USES:
!
  USE QFYAML_Mod
  USE RoundOff_Mod

  IMPLICIT NONE
!
! !LOCAL VARIABLES:
!
  ! Scalars
  REAL(yp)              :: v_real
  LOGICAL               :: v_bool
  INTEGER               :: v_int
  INTEGER               :: RC

  ! Strings
  CHARACTER(LEN=255)    :: v_str
  CHARACTER(LEN=255)    :: key
  CHARACTER(LEN=255)    :: fileName

  ! Arrays
  REAL(yp), allocatable :: my_reals(:)

  ! Objects
  TYPE(QFYAML_t)        :: yml
!
! !REVISION HISTORY:
!  06 Jan 2015 - R. Yantosca - Initial version
!  See the subsequent Git history with the gitk browser!
!EOP
!------------------------------------------------------------------------------
!BOC
  !=========================================================================
  ! Test_QFYAML begins here!
  !=========================================================================

  RC = QFYAML_SUCCESS

  ! Read the YAML file into a config object
  fileName = "input.yml"
  PRINT*, "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml, RC ) 

  ! Read the author%age field
  key   = "author%age"
  v_int = -999
  CALL QFYAML_Add_Get( yml, key, v_int, "" )
  WRITE( 6, '(a30, " | ", i7)') TRIM(key), v_int

  ! Read the author%age field
  key    = "author%lots_of_work"
  v_bool = .FALSE.
  CALL QFYAML_Add_Get( yml, key, v_bool, "" )
  WRITE( 6, '(a30, " | ", l7)') TRIM(key), v_bool

  ! Read the author%age field
  key    = "weather%humidity"
  v_bool = .FALSE.
  CALL QFYAML_Add_Get( yml, key, v_real, "" )
  WRITE( 6, '(a30, " | ", f13.6)') TRIM(key), RoundOff( v_real, 2 )

  print*, '### finishing'
  CALL QFYAML_CleanUp( yml )

END PROGRAM test_qfyaml
