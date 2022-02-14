!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test_Input_Options
!
! !DESCRIPTION: Test program for reading the GEOS-Chem configuration file
!  in YAML format.
!\\
!\\
! !INTERFACE:
!
PROGRAM Test_Input_Options
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
    fileName = "input_options.yml"
    RC       = QFYAML_Success

    ! Read the YAML file
    CALL Init( fileName, yml, yml_anchored, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Init!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Simulation( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Simulation!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Grid( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Grid!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Timesteps( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Timesteps!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Chemistry( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Chemistry!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Convection_PBL_WetDep( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Convection_PBL_WetDep!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_DryDep( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_DryDep!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Photolysis( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Photolysis!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_RRTMG( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_RRTMG!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Transport( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Transport!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Aerosols( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Aerosols!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Strat_Aerosols( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Aerosols!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_Obspack( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Strat_Aerosols!'
       CALL EXIT( -1 )
    ENDIF

    CALL Parse_PlaneFlight( yml, RC )
    IF ( RC /= QFYAML_Success ) THEN
       PRINT*, 'Error encountered in Parse_Strat_Aerosols!'
       CALL EXIT( -1 )
    ENDIF


    ! Cleanup & quit
999 CONTINUE
    print*, "### finishing"
    CALL QFYAML_CleanUp( yml          )
    CALL QFYAML_CleanUp( yml_anchored )

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
    tags(1) = "%name"
    tags(2) = "%start"
    tags(3) = "%end"
    tags(4) = "%data_dir"
    tags(5) = "%met_field"
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

       ! simulation%name
       IF ( INDEX( key, tags(1) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! simulation%start
       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_int_2

       ! simulation%end
       ELSE IF ( INDEX( key, tags(3) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_int_2

       ! simulation%data_dir
       ELSE IF ( INDEX( key, tags(4) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! simulation%met_field
       ELSE IF ( INDEX( key, tags(5) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! simulation%species_database_file
       ELSE IF ( INDEX( key, tags(6) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! simulation%debug_printout
       ELSE IF ( INDEX( key, tags(7) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! simulation%use_gcclassic_timers
       ELSE IF ( INDEX( key, tags(8) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool
       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Simulation!"
    RETURN
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

       ! grid%resolution
       IF ( INDEX( key, tags(1) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! grid%longitude_range
       ELSE IF ( INDEX( key, tags(2) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_real_2

       ! grid%center_lon_at_180
       ELSE IF ( INDEX( key, tags(3) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! grid%latitude_range
       ELSE IF ( INDEX( key, tags(4) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_real_2

       ! grid%half_size_polar_boxes
       ELSE IF ( INDEX( key, tags(5) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! grid%number_of_levels
       ELSE IF ( INDEX( key, tags(6) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_int

       ! grid%nested_grid_simulation
       ELSE IF ( INDEX( key, tags(7) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! grid%buffer_zone_NSEW
       ELSE IF ( INDEX( key, tags(8) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_4, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_int_4

       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Grid!"
    RETURN
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
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_int

       ELSE IF ( INDEX( key, "%chemistry_and_emissions" ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_int

       ELSE
          ! Pass

       ENDIF
    ENDDO

    print*, '---'
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Grid!"

  END SUBROUTINE Parse_Timesteps

  SUBROUTINE Parse_Chemistry( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    !
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(5)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool
    REAL(yp)           :: v_real

    !
    RC       = QFYAML_Success
    tags(1)  = "activate"
    tags(2)  = "linear_chemistry_aloft%activate"
    tags(3)  = "linear_chemistry_aloft%use_linoz_for_O3"
    tags(4)  = "use_static_H2O_bc"
    tags(5)  = "gamma_HO2"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool  = MISSING_BOOL
       v_real  = MISSING_REAL
       ! Search key
       key = "operations%chemistry%" // TRIM( tags(N) )

       ! %chemistry%activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %chemistry%linear_chemistry_aloft%activate"
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %chemistry%linear_chemistry_aloft%use_linoz_for_O3"
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %chemistry%use_static_H2O_bc
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %chemistry%gamma_HO2
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ENDIF
    ENDDO

    PRINT*
    RETURN

    ! Exit w/ error
999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Chemistry"

  END SUBROUTINE Parse_Chemistry

  SUBROUTINE Parse_Convection_PBL_WetDep( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    !
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(4)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool

    !
    RC      = QFYAML_Success
    tags(1) = "convection%activate"
    tags(2) = "pbl_mixing%activate"
    tags(3) = "pbl_mixing%use_non_local_pbl"
    tags(4) = "wet_deposition%activate"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool  = MISSING_BOOL

       ! Search key
       key = "operations%" // TRIM( tags(N) )

       ! %chemistry%activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %pbl_mixing%activate
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %pbl_mixing%use_non_local_pbl
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %wet_deposition%activate
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ENDIF
    ENDDO

    PRINT*
    RETURN

    ! Exit w/ error
999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Convection_WetDep"

  END SUBROUTINE Parse_Convection_PBL_WetDep

  SUBROUTINE Parse_DryDep( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    !
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(5)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool
    REAL(yp)           :: v_real

    !
    RC      = QFYAML_Success
    tags(1) = "%dry_deposition%activate"
    tags(2) = "%dry_deposition%CO2_effect"
    tags(3) = "%dry_deposition%CO2_level"
    tags(4) = "%dry_deposition%reference_CO2_level"
    tags(5) = "%dry_deposition%diag_alt_above_sfc_in_m"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool  = MISSING_BOOL
       v_real  = MISSING_REAL

       ! Search key
       key = "operations" // TRIM( tags(N) )

       ! %dry_deposition%activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %dry_deposition%CO2_effect
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %dry_deposition%CO2_level
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %dry_deposition%reference_CO2_level
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %dry_deposition%diag_alt_above_sfc_in_m
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real
       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Grid!"
    RETURN
  END SUBROUTINE Parse_DryDep


  SUBROUTINE Parse_Photolysis( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    !
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(4)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool
    INTEGER            :: v_int
    CHARACTER(LEN=255) :: v_str

    !
    RC      = QFYAML_Success
    tags(1) = "%photolysis%input_directory"
    tags(2) = "%photolysis%overhead_O3%use_online_O3_from_model"
    tags(3) = "%photolysis%overhead_O3%use_column_O3_from_met"
    tags(4) = "%photolysis%overhead_O3%use_TOMS_SBUV_O3"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool  = MISSING_BOOL
       v_str   = MISSING_STR

       ! Search key
       key = "operations" // TRIM( tags(N) )

       ! %photolysis%input_directory"
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! %photolysis%overhead_O3%use_online_O3_from_model
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %photolysis%overhead_O3%use_column_O3_from_met
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %photolysis%use_TOMS_SBUV_O3
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ENDIF
    ENDDO

    print*, '---'
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Photolysis"
    RETURN
  END SUBROUTINE Parse_Photolysis

  SUBROUTINE Parse_RRTMG( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT) :: RC
    !
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(7)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool
    INTEGER            :: v_int

    !
    RC      = QFYAML_Success
    tags(1) = "%rrtmg%activate"
    tags(2) = "%rrtmgAOD_wavelength_in_nm"
    tags(3) = "%rrtmg%longwave_fluxes"
    tags(4) = "%rrtmg%shortwave_fluxes"
    tags(5) = "%rrtmg%clear_sky_flux"
    tags(6) = "%rrtmg%all_sky_flux"
    tags(7) = "%rrtmg%radiation_timestep_in_s"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool = MISSING_BOOL
       v_int  = MISSING_INT

       ! Search key
       key = "operations" // TRIM( tags(N) )

       ! %rrtmg%activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %rrtmg%AOD_wavelength_in_nm
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_int

       ! %rrtmg%longwave_fluxes
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %rrtmg%shortwave_fluxes
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %rrtmg%clear_sky_flux
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %rrtmg%all_sky_flux
       ELSE IF ( INDEX( key, TRIM( tags(6) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %rrtmg%shortwave_fluxes
       ELSE IF ( INDEX( key, TRIM( tags(7) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_int, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_int

       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_RRTMG

  SUBROUTINE Parse_Transport( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT)    :: RC
    !
    INTEGER            :: C
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(4)
    CHARACTER(LEN=255) :: key
    INTEGER            :: a_int_3(3)
    LOGICAL            :: v_bool
    INTEGER            :: v_int
    CHARACTER(LEN=255) :: v_str
    REAL(yp)           :: v_real
    CHARACTER(LEN=14)  :: a_str(300)

    !
    RC      = QFYAML_Success
    tags(1) = "%transport%activate"
    tags(2) = "%transport%fill_negative_values"
    tags(3) = "%transport%iord_jord_kord"
    tags(4) = "%transport%transported_species"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       a_int_3 = MISSING_INT
       v_bool  = MISSING_BOOL
       a_str   = MISSING_STR

       ! Search key
       key = "operations" // TRIM( tags(N) )

       ! %transport%activate"
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %transport%fill_negative_values
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %transport%iord_jord_kord
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_int_3, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_int_3

       ! %transport%transported_species
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_str, "", RC, dynamic_size=.TRUE. )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_str

       ENDIF
    ENDDO

    ! Find the number of species
    C = 0
    DO N = 1, SIZE( a_str )
       IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT
       C = C + 1
    ENDDO
    PRINT*, 'Number of transported species: '
    PRINT*, '==> ', C
    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_Transport

  SUBROUTINE Parse_Aerosols( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT)    :: RC
    !
    INTEGER            :: C
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(12)
    CHARACTER(LEN=255) :: key
    REAL(yp)           :: a_real(2)
    LOGICAL            :: v_bool
    REAL(yp)           :: v_real

    !
    RC       = QFYAML_Success
    tags(1 ) = "%carbon%activate"
    tags(2 ) = "%carbon%brown_carbon"
    tags(3 ) = "%complex_SOA%activate"
    tags(4 ) = "%complex_SOA%semivolatile_POA"
    tags(5 ) = "%dust%activate"
    tags(6 ) = "%dust%acid_uptake_on_dust"
    tags(7 ) = "%sea_salt%activate"
    tags(8 ) = "%sea_salt%SALA_radius_bin_in_um"
    tags(9 ) = "%sea_salt%SALC_radius_bin_in_um"
    tags(10) = "%sea_salt%marine_organic_aerosols"
    tags(11) = "%sulfate%activate"
    tags(12) = "%sulfate%metal_cat_SO2_oxidation"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       a_real = MISSING_REAL
       v_bool = MISSING_BOOL

       ! Search key
       key = "aerosols" // TRIM( tags(N) )

       ! %carbon%activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %carbon%brown_carbon
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %complex_SOA%activate
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %complex_SOA%semivolatile_POA
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %dust%activate
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %dust%acid_uptake_on_dust
       ELSE IF ( INDEX( key, TRIM( tags(6) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %sea_salt%activate
       ELSE IF ( INDEX( key, TRIM( tags(7) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %sea_salt%SALA_radius_bin_in_um
       ELSE IF ( INDEX( key, TRIM( tags(8) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          a_real(1) = Roundoff( a_real(1), 2 )
          a_real(2) = Roundoff( a_real(2), 2 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_real

       ! %sea_salt%SALC_radius_bin_in_um
       ELSE IF ( INDEX( key, TRIM( tags(9) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          a_real(1) = Roundoff( a_real(1), 2 )
          a_real(2) = Roundoff( a_real(2), 2 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_real

       ! %sea_salt%marine_organic_aerosols
       ELSE IF ( INDEX( key, TRIM( tags(10) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %sulfate%activate
       ELSE IF ( INDEX( key, TRIM( tags(11) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %sulfate%metal_cat_SO2_oxidation
       ELSE IF ( INDEX( key, TRIM( tags(12) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_Aerosols

  SUBROUTINE Parse_Strat_Aerosols( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT)    :: RC
    !
    INTEGER            :: C
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(14)
    CHARACTER(LEN=255) :: key
    REAL(yp)           :: a_real(2)
    LOGICAL            :: v_bool
    REAL(yp)           :: v_real

    !
    RC       = QFYAML_Success
    tags(1 ) = "%settle_strat_aerosol"
    tags(2 ) = "%polar_strat_clouds%activate"
    tags(3 ) = "%polar_strat_clouds%het_chem"
    tags(4 ) = "%homogeneous_NAT"
    tags(5 ) = "%NAT_supercooling_req_in_K"
    tags(6 ) = "%calc_strat_aero_optdepth"
    tags(7 ) = "%enhance_BC_absorption%activate"
    tags(8 ) = "%enhance_BC_absorption%hydrophilic_BC"
    tags(9 ) = "%enhance_BC_absorption%hydrophobic_BC"
    tags(10) = "%photolyze_nitrate%activate"
    tags(11) = "%photolyze_nitrate%NITs_Jscale_JHNO3"
    tags(12) = "%photolyze_nitrate%NIT_Jscale_JHNO2"
    tags(13) = "%photolyze_nitrate%percent_channel_A_HONO"
    tags(14) = "%photolyze_nitrate%percent_channel_B_NO2"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool = MISSING_BOOL
       v_real = MISSING_REAL

       ! Search key
       key = "aerosols%stratosphere" // TRIM( tags(N) )

       ! %settle_strat_aerosol
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %polar_strat_clouds%activate
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %polar_strat_clouds%het_chem
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %homogeneous_NAT
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %NAT_supercooling_req_in_K
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %calc_strat_aero_optdepth
       ELSE IF ( INDEX( key, TRIM( tags(6) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %enhance_BC_absorption%activate
       ELSE IF ( INDEX( key, TRIM( tags(7) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %enhance_BC_absoprtion%hydrophilic_BC
       ELSE IF ( INDEX( key, TRIM( tags(8) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %enhance_BC_absoprtion%hydrophobic_BC
       ELSE IF ( INDEX( key, TRIM( tags(9) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %photolyze_nitrate%activate
       ELSE IF ( INDEX( key, TRIM( tags(10) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %photolyze_nitrate%NITS_Jscale_JHNO3
       ELSE IF ( INDEX( key, TRIM( tags(11) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %photolyze_nitrate%NIT_Jscale_JHNO2
       ELSE IF ( INDEX( key, TRIM( tags(12) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %photolyze_nitrate%percent_channel_A_HONO
       ELSE IF ( INDEX( key, TRIM( tags(13) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ! %photolyze_nitrate%percent_channel_B_NO2
       ELSE IF ( INDEX( key, TRIM( tags(14) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          v_real = Roundoff( v_real, 3 )
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_real

       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_Strat_Aerosols

  SUBROUTINE Parse_ObsPack( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT)    :: RC
    !
    INTEGER            :: C
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(5)
    CHARACTER(LEN=255) :: key
    REAL(yp)           :: a_real(2)
    LOGICAL            :: v_bool
    REAL(yp)           :: v_real
    CHARACTER(LEN=255) :: v_str
    CHARACTER(LEN=14)  :: a_str(50)

    !
    RC       = QFYAML_Success
    tags(1 ) = "%activate"
    tags(2 ) = "%quiet_logfile_output"
    tags(3 ) = "%input_file"
    tags(4 ) = "%output_file"
    tags(5 ) = "%output_species"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       a_str  = MISSING_STR
       v_str  = MISSING_STR
       v_bool = MISSING_BOOL
       v_real = MISSING_REAL

       ! Search key
       key = "extra_diagnostics%obspack" // TRIM( tags(N) )

       ! %activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %quiet_logfile_output
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %input_file
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! %output_file
       ELSE IF ( INDEX( key, TRIM( tags(4) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! %species
       ELSE IF ( INDEX( key, TRIM( tags(5) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, a_str, "", RC, dynamic_size=.TRUE. )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', a_str

       ENDIF
    ENDDO

    ! Find the number of species
    C = 0
    DO N = 1, SIZE( a_str )
       IF ( TRIM( a_str(N) ) == MISSING_STR ) EXIT
       C = C + 1
    ENDDO
    PRINT*, 'Number of ObsPack species: '
    PRINT*, '==> ', C
    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_ObsPack

  SUBROUTINE Parse_PlaneFlight( yml, RC )
    !
    TYPE(QFYAML_t),  INTENT(INOUT)  :: yml
    INTEGER,         INTENT(OUT)    :: RC
    !
    INTEGER            :: C
    INTEGER            :: N
    CHARACTER(LEN=255) :: tags(3)
    CHARACTER(LEN=255) :: key
    LOGICAL            :: v_bool
    CHARACTER(LEN=255) :: v_str

    !
    RC       = QFYAML_Success
    tags(1 ) = "%activate"
    tags(2 ) = "%input_file"
    tags(3 ) = "%output_file"

    ! Loop over the number of tags in the species database
    DO N = 1, SIZE( tags )

       ! Set intial values to default "missing" values
       ! This will force creation of variables with these values
       v_bool = MISSING_BOOL
       v_str  = MISSING_STR

       ! Search key
       key = "extra_diagnostics%planeflight" // TRIM( tags(N) )

       ! %activate
       IF ( INDEX( key, TRIM( tags(1) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_bool

       ! %input_file
       ELSE IF ( INDEX( key, TRIM( tags(2) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', TRIM( v_str )

       ! %output_file
       ELSE IF ( INDEX( key, TRIM( tags(3) ) ) > 0 ) THEN
          CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
          IF ( RC /= QFYAML_Success ) GOTO 999
          PRINT*, TRIM( key )
          PRINT*, '==> ', v_str
       ENDIF
    ENDDO

    PRINT*
    RETURN

999 CONTINUE
    RC = QFYAML_Failure
    print*, "Error in Parse_Transport"
    RETURN
  END SUBROUTINE Parse_PlaneFlight

END PROGRAM Test_Input_Options
