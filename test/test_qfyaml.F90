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
  REAL(yp)                        :: v_real
  LOGICAL                         :: v_bool
  INTEGER                         :: v_int
  INTEGER                         :: RC
  INTEGER                         :: N
  INTEGER                         :: match_ct

  ! Strings
  CHARACTER(LEN=255)              :: v_str
  CHARACTER(LEN=255)              :: key
  CHARACTER(LEN=255)              :: fileName

  ! Arrays
  REAL(yp),           ALLOCATABLE :: a_real(:)
  CHARACTER(LEN=255), ALLOCATABLE :: a_str(:)
  CHARACTER(LEN=100)              :: match_vars(5)

  ! Objects
  TYPE(QFYAML_t)                  :: yml
  TYPE(QFYAML_t)                  :: yml_anchored
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
  fileName = "qfyaml.yml"
  WRITE( 6, "(a)" ) "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml, yml_anchored, RC )

  ! Read various fields
  WRITE( 6, '(/, a)' ) "### YAML VARIABLES"

  key   = "author%age"
  v_int = -999
  CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
  WRITE( 6, "(a32, "" : "", i7)") ADJUSTL(key), v_int

  key    = "author%fav_reals"
  ALLOCATE( a_real(2) )
  a_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
  WRITE( 6, "(a32, "" : "", 2f7.2)") ADJUSTL(key), a_real
  DEALLOCATE( a_real )

  key    = "author%more_reals"
  ALLOCATE( a_real(4) )
  a_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
  WRITE( 6, "(a32, "" : "", 4f11.6)") ADJUSTL(key), a_real
  DEALLOCATE( a_real )

  key    = "author%lots_of_work"
  v_bool = .FALSE.
  CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
  WRITE( 6, "(a32, "" : "", l7)") ADJUSTL(key), v_bool

  key    = "author_name%first"
  v_str  = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a32, "" : "", a)") ADJUSTL(key), TRIM(v_str)

  key    = "author_name%full"
  v_str  = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a32, "" : "", a)") ADJUSTL(key), TRIM(v_str)

  key    = "filename"
  v_str = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a32, "" : "", a)") ADJUSTL(key), TRIM(v_str)

  key    = "weather%humidity"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a32, "" : "", f13.6)") ADJUSTL(key), v_real

  key    = "weather%temperature%daily"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a32, "" : "", f13.6)") ADJUSTL(key), v_real

  key    = "weather%temperature%weekly%units"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a32, "" : "", a)") ADJUSTL(key), TRIM(v_str)

  key    = "weather%pressure"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a32, "" : "", f13.6)") ADJUSTL(key), v_real

  WRITE( 6, '(/, a)' ) '### FIND NEXT-HIGHER VARIABLES IN "weather"'

  CALL QFYAML_FindNextHigher( yml, "weather%", match_ct, match_vars )

  DO N = 1, match_ct
     print*, '>>>', N, TRIM( match_vars(N) )
  ENDDO

  WRITE( 6, '(/, a)' ) "### YAML SEQUENCES"

  key   = "fruits"
  ALLOCATE( a_str(3) )
  a_str = ""
  CALL QFYAML_Add_Get( yml, key, a_str, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str )
     print*, N, TRIM( a_str(N) )
  ENDDO
  DEALLOCATE( a_str )
  PRINT*

  key   = "more_fruits%p_fruits"
  ALLOCATE( a_str(4) )
  a_str = ""
  CALL QFYAML_Add_Get( yml, key, a_str, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str )
     print*, N, TRIM( a_str(N) )
  ENDDO
  DEALLOCATE( a_str )
  WRITE( 6, "(a)" )


  key   = "even_more_fruits%exotic_fruits%hard_to_find"
  ALLOCATE( a_str(5) )
  a_str = ""
  CALL QFYAML_Add_Get( yml, key, a_str, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str )
     print*, N, TRIM( a_str(N) )
  ENDDO
  DEALLOCATE( a_str )
  WRITE( 6, "(a)" )

  ! Finalize the config object
  WRITE( 6, "(a)" ) "#### finishing"
  CALL QFYAML_CleanUp( yml          )
  CALL QFYAML_CleanUp( yml_anchored )

END PROGRAM test_qfyaml
