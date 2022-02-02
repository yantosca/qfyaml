!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test_GeosChem_Config
!
! !DESCRIPTION: Test program for reading the GEOS-Chem configuration file
!  in YAML format.
!\\
!\\
! !INTERFACE:
!
PROGRAM Test_GeosChem_Config
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
  INTEGER                     :: RC
  CHARACTER(LEN=255)          :: fileName

  ! Objects
  TYPE(QFYAML_t)              :: yml
  TYPE(QFYAML_t)              :: yml_anchored
!
! !DEFINED PARAMETERS:
!
  INTEGER,          PARAMETER :: MISSING_INT  = -999
  REAL(yp),         PARAMETER :: MISSING_MW   = 1.0_yp
  REAL(yp),         PARAMETER :: MISSING_REAL = -999e+0_yp
  LOGICAL,          PARAMETER :: MISSING_BOOL = .FALSE.
  REAL(yp),         PARAMETER :: MISSING_VV   = 1.0e-20_yp
  REAL(yp),         PARAMETER :: ZERO         = 0.0_yp
  REAL(yp),         PARAMETER :: ONE          = 1.0_yp
  CHARACTER(LEN=7), PARAMETER :: MISSING_STR  = 'UNKNOWN'
!
! !REVISION HISTORY:
!  06 Jan 2015 - R. Yantosca - Initial version
!  See the subsequent Git history with the gitk browser!
!EOP
!------------------------------------------------------------------------------
!BOC

    ! Read the YAML file into a config object
    fileName = "geoschem_config.yml"
    RC       = QFYAML_Success

    ! Read the YAML file
    CALL Init( fileName, yml, yml_anchored, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Init!'
       CALL EXIT( -1 )
    ENDIF

    ! Parse the simulation section
    CALL Parse_Simulation( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Simulation!'
       CALL EXIT( -1 )
    ENDIF

    ! Parse the simulation section
    CALL Parse_Grid( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Simulation!'
       CALL EXIT( -1 )
    ENDIF

    ! Parse the simulation section
    CALL Parse_Timesteps( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Timesteps!'
       CALL EXIT( -1 )
    ENDIF

    print*, "### finishing"
    CALL QFYAML_CleanUp( yml          )
    CALL QFYAML_CleanUp( yml_anchored )

  ! FORMAT statements (for use in code below)
110 FORMAT( a45, " | ", a      )
210 FORMAT( a45, " | ", L10    )
310 FORMAT( a45, " | ", i10    )
320 FORMAT( a45, " | ", 2i10   )
330 FORMAT( a45, " | ", 3i10   )
410 FORMAT( a45, " | ", f10.2  )
420 FORMAT( a45, " | ", 2f10.2 )
430 FORMAT( a45, " | ", 3f10.2 )

CONTAINS

  SUBROUTINE Init( file_name, yml, yml_anchored, RC )
    !
    CHARACTER(LEN=*), INTENT(IN)  :: file_name
    TYPE(QFYAML_t),   INTENT(OUT) :: yml
    TYPE(QFYAML_t),   INTENT(OUT) :: yml_anchored
    INTEGER,          INTENT(OUT) :: RC
    !
    RC = QFYAML_Success
    PRINT*, "### Reading " // TRIM( fileName )
    CALL QFYAML_Init( fileName, yml, yml_anchored, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in QFYAML_Init...exiting'
       RC = QFYAML_Failure
       RETURN
    ENDIF
  END SUBROUTINE Init

  SUBROUTINE Parse_Simulation( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    ! 
    INTEGER            :: N
    LOGICAL            :: v_bool
    CHARACTER(LEN=30)  :: tags(8)
    CHARACTER(LEN=80)  :: key
    CHARACTER(LEN=255) :: v_str
    INTEGER            :: a_int_2(2)
    !
    RC      = QFYAML_Success
    tags(1) = "%start"
    tags(2) = "%end"
    tags(3) = "%data_dir"
    tags(4) = "%met_field"
    tags(5) = "%name"
    tags(6) = "%species_database_file"
    tags(7) = "%debug_printout"
    tags(8) = "%use_gcclassic_timers"
    
    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       a_int_2 = MISSING_INT
       v_bool  = MISSING_BOOL
       v_str   = MISSING_STR
       
       ! Search key
       key = "simulation" // TRIM( tags(N) )

       ! Save into the proper field of the species database
       IF ( INDEX( key, "%start" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 320 ) TRIM( key ), a_int_2
          
       ELSE IF ( INDEX( key, "%end" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 320 ) TRIM( key ), a_int_2
          
       ELSE IF ( INDEX( key, "%met_field" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 110 ) TRIM( key ), TRIM( v_str )
          
       ELSE IF ( INDEX( key, "%name" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 110 ) TRIM( key ), TRIM( v_str )
          
       ELSE IF ( INDEX( key, "%species_database_file" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 110 ) TRIM( key ), TRIM( v_str )
          
       ELSE IF ( INDEX( key, "%debug_printout" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool
          
       ELSE IF ( INDEX( key, "%use_gcclassic_timers" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool

       ELSE
          ! Pass
          
       ENDIF
    ENDDO

    print*, '---'
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Simulation!"
    RETURN

    ! FORMAT statements (for use in code below)
110 FORMAT( a45, " | ", a      )
210 FORMAT( a45, " | ", L10    )
310 FORMAT( a45, " | ", i10    )
320 FORMAT( a45, " | ", 2i10   )
330 FORMAT( a45, " | ", 3i10   )
340 FORMAT( a45, " | ", 4i10   )
410 FORMAT( a45, " | ", f10.2  )
420 FORMAT( a45, " | ", 2f10.2 )
430 FORMAT( a45, " | ", 3f10.2 )
440 FORMAT( a45, " | ", 4f10.2 )

  END SUBROUTINE Parse_Simulation

  SUBROUTINE Parse_Grid( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    ! 
    INTEGER            :: N
    LOGICAL            :: v_bool
    CHARACTER(LEN=30)  :: tags(8)
    CHARACTER(LEN=80)  :: key
    CHARACTER(LEN=255) :: v_str
    INTEGER            :: v_int
    INTEGER            :: a_int_2(2)
    INTEGER            :: a_int_4(4)
    REAL(yp)           :: a_real_2(2)
    !
    RC      = QFYAML_Success
    tags(1) = "%resolution"
    tags(2) = "%longitude_range"
    tags(3) = "%center_lon_at_180"
    tags(4) = "%latitude_range"
    tags(5) = "%half_size_polar_boxes"
    tags(6) = "%number_of_levels"
    tags(7) = "%nested_grid_simulation"
    tags(8) = "%buffer_zone_NSEW"
    
    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       a_int_2  = MISSING_INT
       a_int_4  = MISSING_INT
       a_real_2 = MISSING_REAL
       v_bool   = MISSING_BOOL
       v_str    = MISSING_STR
       
       ! Search key
       key = "grid" // TRIM( tags(N) )

       ! Save into the proper field of the species database
       IF ( INDEX( key, "%resolution" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 110 ) TRIM( key ), TRIM( v_str )
          
       ELSE IF ( INDEX( key, "%longitude_range" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 420 ) TRIM( key ), a_real_2
          
       ELSE IF ( INDEX( key, "%center_lon_at_180" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool
          
       ELSE IF ( INDEX( key, "%latitude_range" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 420 ) TRIM( key ), a_real_2
          
       ELSE IF ( INDEX( key, "%half_size_polar_boxes" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool
          
       ELSE IF ( INDEX( key, "%number_of_levels" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 310 ) TRIM( key ), v_int
          
       ELSE IF ( INDEX( key, "%nested_grid_simulation" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool

       ELSE IF ( INDEX( key, "%buffer_zone_NSEW" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_4, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 340 ) TRIM( key ), a_int_4

       ELSE
          ! Pass
          
       ENDIF
    ENDDO

    print*, '---'
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Grid!"
    RETURN

    ! FORMAT statements (for use in code below)
110 FORMAT( a45, " | ", a      )
210 FORMAT( a45, " | ", L10    )
310 FORMAT( a45, " | ", i10    )
320 FORMAT( a45, " | ", 2i10   )
330 FORMAT( a45, " | ", 3i10   )
340 FORMAT( a45, " | ", 4i10   )
410 FORMAT( a45, " | ", f10.2  )
420 FORMAT( a45, " | ", 2f10.2 )
430 FORMAT( a45, " | ", 3f10.2 )
440 FORMAT( a45, " | ", 3f10.2 )

  END SUBROUTINE Parse_Grid

  SUBROUTINE Parse_Timesteps( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    ! 
    INTEGER            :: N
    LOGICAL            :: v_bool
    CHARACTER(LEN=30)  :: tags(2)
    CHARACTER(LEN=80)  :: key
    INTEGER            :: v_int
    !
    RC      = QFYAML_Success
    tags(1) = "%transport_and_convection"
    tags(2) = "%chemistry_and_emissions"
    
    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_int = MISSING_INT
       
       ! Search key
       key = "timesteps" // TRIM( tags(N) )

       ! Save into the proper field of the species database
       IF ( INDEX( key, "%transport_and_convection" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 310 ) TRIM( key ), v_int
          
       ELSE IF ( INDEX( key, "%chemistry_and_emissions" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 310 ) TRIM( key ), v_int
 
       ELSE
          ! Pass
          
       ENDIF
    ENDDO

    print*, '---'
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Grid!"
    RETURN

    ! FORMAT statements (for use in code below)
310 FORMAT( a45, " | ", i10    )

  END SUBROUTINE Parse_Timesteps

END PROGRAM Test_GeosChem_Config
