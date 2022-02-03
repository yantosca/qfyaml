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

    ! Parse the simulation section
    CALL Parse_Operations( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Operations!'
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

  SUBROUTINE Parse_Operations( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    ! 
    INTEGER            :: N
    CHARACTER(LEN=80)  :: tags(21)
    CHARACTER(LEN=80)  :: key
    LOGICAL            :: v_bool
    INTEGER            :: v_int
    CHARACTER(LEN=255) :: v_str
    REAL(yp)           :: v_real

    !
    RC       = QFYAML_Success
    tags(1)  = "%chemistry%activate"
    tags(2)  = "%chemistry%linear_chemistry_aloft%activate"
    tags(3)  = "%chemistry%linear_chemistry_aloft%use_linoz_for_O3"
    tags(4)  = "%chemistry%use_static_H2O_bc"
    tags(5)  = "%chemistry%gamma_HO2"
    tags(6)  = "%convection%activate"
    tags(7)  = "%dry_deposition%activate"
    tags(8)  = "%dry_deposition%co2_effect"
    tags(9)  = "%dry_deposition%co2_level"
    tags(10) = "%dry_deposition%reference_co2_level"
    tags(11) = "%dry_deposition%diag_alt_above_sfc_in_m"
    tags(12) = "%pbl_mixing%activate"
    tags(13) = "%pbl_mixing%use_non_local_pbl"
    tags(14) = "%photolysis%input_directory"
    tags(15) = "%photolysis%overhead_O3%use_online_O3_from_model"
    tags(16) = "%photolysis%overhead_O3%use_column_O3_from_met"
    tags(17) = "%photolysis%overhead_O3%use_TOMS_SBUV_O3"
    tags(18) = "%transport%activate"
    tags(19) = "%transport%fill_negative_values"
    tags(20) = "%transport%iord_jord_kord"
    tags(21) = "%transport%activate"
    
    ! Loop over the number of tags in the species database
    DO N = 1, 1  !SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
        ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool   = MISSING_BOOL
       v_real   = MISSING_REAL
       v_str    = MISSING_STR
       v_int    = MISSING_INT
       
       ! Search key
       key = "operations" // TRIM( tags(N) )
       print*, trim(key)

       ! %chemistry%activate
       IF ( INDEX( key, tags(1) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          print*, '### v_bool', v_bool
          IF ( RC /= QFYAML_Success ) GOTO 999
          WRITE( 6, 210 ) TRIM( key ), v_bool

!       ! %chemistry%linear_chemistry_aloft%activate"
!       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%linear_chemistry_aloft%use_linoz_for_O3"
!       ELSE IF ( INDEX( key, tags(3) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%use_static_H2O_bc
!       ELSE IF ( INDEX( key, tags(4) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%gamma_HO2
!       ELSE IF ( INDEX( key, tags(5) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 410 ) TRIM( key ), v_bool
!
!       ! %convection%activate
!       ELSE IF ( INDEX( key, tags(6) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%linear_chemistry_aloft%activate"
!       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%linear_chemistry_aloft%activate"
!       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
!
!       ! %chemistry%linear_chemistry_aloft%activate"
!       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
!          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
!          IF ( RC /= QFYAML_Success ) GOTO 999
!          WRITE( 6, 210 ) TRIM( key ), v_bool
 
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
    ! FORMAT statements (for use in code below)
110 FORMAT( a60, " | ", a      )
210 FORMAT( a60, " | ", L10    )
310 FORMAT( a60, " | ", i10    )
320 FORMAT( a60, " | ", 2i10   )
330 FORMAT( a60, " | ", 3i10   )
340 FORMAT( a60, " | ", 4i10   )
410 FORMAT( a60, " | ", f10.2  )
420 FORMAT( a60, " | ", 2f10.2 )
430 FORMAT( a60, " | ", 3f10.2 )
440 FORMAT( a60, " | ", 3f10.2 )

  END SUBROUTINE Parse_Operations

END PROGRAM Test_GeosChem_Config
