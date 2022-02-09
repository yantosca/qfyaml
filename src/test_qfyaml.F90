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

  ! Strings
  CHARACTER(LEN=255)              :: v_str
  CHARACTER(LEN=255)              :: key
  CHARACTER(LEN=255)              :: fileName

  ! Arrays
  REAL(yp),           ALLOCATABLE :: a_real(:)
  CHARACTER(LEN=255)              :: a_str_3(3)
  CHARACTER(LEN=255)              :: a_str_4(4)
  CHARACTER(LEN=255)              :: a_str_5(5)

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
  fileName = "input.yml"
  PRINT*, "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml, yml_anchored, RC ) 

  ! Read various fields

  key   = "author%age"
  v_int = -999
  CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
  WRITE( 6, "(a30, "" | "", i7)") TRIM(key), v_int

  key    = "author%fav_reals"
  ALLOCATE( a_real(2) )
  a_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
  WRITE( 6, "(a30, "" | "", 2f7.2)") TRIM(key), a_real
  DEALLOCATE( a_real )

  key    = "author%more_reals"
  ALLOCATE( a_real(4) )
  a_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
  WRITE( 6, "(a30, "" | "", 4f11.6)") TRIM(key), a_real
  DEALLOCATE( a_real )

  key    = "author%lots_of_work"
  v_bool = .FALSE.
  CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
  WRITE( 6, "(a30, "" | "", l7)") TRIM(key), v_bool

  key    = "author_name%first"
  v_str  = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a30, "" | "", a)") TRIM(key), TRIM(v_str)

  key    = "author_name%full"
  v_str  = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a30, "" | "", a)") TRIM(key), TRIM(v_str)

  key    = "filename"
  v_str = ""
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a30, "" | "", a)") TRIM(key), TRIM(v_str)

  key    = "weather%humidity"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a30, "" | "", f13.6)") TRIM(key), v_real

  key    = "weather%temperature%daily"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a30, "" | "", f13.6)") TRIM(key), v_real

  key    = "weather%temperature%weekly%units"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
  WRITE( 6, "(a30, "" | "", a)") TRIM(key), TRIM(v_str)

  key    = "weather%pressure"
  v_real = -999.0_yp
  CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
  WRITE( 6, "(a30, "" | "", f13.6)") TRIM(key), v_real

  print*
  print*, '### YAML SEQUENCES'
  key     = "fruits"
  a_str_3 = ""
  CALL QFYAML_Add_Get( yml, key, a_str_3, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str_3 )
     print*, N, TRIM( a_str_3(N) )
  ENDDO

  key     = "more_fruits%p_fruits"
  a_str_4 = ""
  CALL QFYAML_Add_Get( yml, key, a_str_4, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str_4 )
     print*, N, TRIM( a_str_4(N) )
  ENDDO

  key     = "even_more_fruits%exotic_fruits%hard_to_find"
  a_str_5 = ""
  CALL QFYAML_Add_Get( yml, key, a_str_5, "", RC )
  WRITE( 6, "(a)" ) TRIM(key)
  DO N = 1, SIZE( a_str_5 )
     print*, N, TRIM( a_str_5(N) )
  ENDDO

  ! Finalize the config object
  print*, "### finishing"
  CALL QFYAML_CleanUp( yml          )
  CALL QFYAML_CleanUp( yml_anchored )

END PROGRAM test_qfyaml
