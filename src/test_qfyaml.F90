PROGRAM test_qfyaml

  USE QFYAML_Mod

  IMPLICIT NONE

  INTEGER, PARAMETER    :: yp = kind(0.e0)

  TYPE(QFYAML_t)        :: yml

  ! Some dummy variables
  REAL(yp), allocatable :: my_reals(:)
  REAL(yp)              :: v_dble
  LOGICAL               :: v_bool
  INTEGER               :: v_int
  INTEGER               :: RC
  CHARACTER(LEN=255)    :: key
  CHARACTER(len=255)    :: v_str
  CHARACTER(LEN=255)    :: fileName

  ! Variables can be placed inside categories

  ! Read the YML file into a config object
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

  print*, '### finishing'
  CALL QFYAML_CleanUp( yml )

END PROGRAM test_qfyaml
