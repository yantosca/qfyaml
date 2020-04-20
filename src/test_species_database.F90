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
  LOGICAL                     :: v_bool
  INTEGER                     :: v_int
  INTEGER                     :: N
  INTEGER                     :: S
  INTEGER                     :: RC
  REAL(yp)                    :: v_real

  ! Strings
  CHARACTER(LEN=14)           :: tag
  CHARACTER(LEN=14)           :: spc
  CHARACTER(LEN=255)          :: v_str
  CHARACTER(LEN=255)          :: key
  CHARACTER(LEN=255)          :: fileName

  ! Arrays
  REAL(yp),       ALLOCATABLE :: a_real(:)

  ! String arrays
  CHARACTER(LEN=14)           :: tags(41)
  CHARACTER(LEN=14)           :: species(4)

  ! Objects
  TYPE(QFYAML_t)              :: yml
!
! !DEFINED PARAMETERS:
!
  INTEGER,          PARAMETER :: MISSING_INT  = -999
  REAL(yp),         PARAMETER :: MISSING_MW   = 1.0_yp
  REAL(yp),         PARAMETER :: MISSING_REAL = -999e+0_yp
  LOGICAL,          PARAMETER :: MISSING_BOOL = .FALSE.
  REAL(yp),         PARAMETER :: MISSING_VV   = 1.0e-20_yp
  REAL(yp),         PARAMETER :: ZERO         = 0.0_yp
  CHARACTER(LEN=7), PARAMETER :: MISSING_STR  = 'UNKNOWN'
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
  
  tags = (/ "Name          ", "FullName      ", "Formula       ",           &
            "Notes         ", "Is_Advected   ", "Is_Aero       ",           &
            "Is_DryAlt     ", "Is_DryDep     ", "Is_Gas        ",           &
            "Is_HygroGrowth", "Is_ActiveChem ", "Is_FixedChem  ",           &
            "Is_Kpp        ", "Is_Photolysis ", "Is_WetDep     ",           &
            "Is_InRestart  ", "MW_g          ", "EmMW_g        ",           &
            "MolecRatio    ", "BackgroundVV  ", "Density       ",           &
            "Radius        ", "Henry_K0      ", "Henry_CR      ",           &
            "Henry_PKA     ", "DD_AeroDryDep ", "DD_DustDryDep ",           &
            "DD_DvzAerSnow ", "DD_DvzMinVal  ", "DD_F0         ",           &
            "DD_KOA        ", "WD_LiqAndGas  ", "WD_ConvFacI2G ",           &
            "WD_RetFactor  ", "WD_Is_H2SO4   ", "WD_Is_HNO3    ",           &
            "WD_Is_SO2     ", "WD_CoarseAer  ", "WD_AerScavEff ",           &
            "WD_KcScaleFac ", "WD_RainoutEff "                            /)

  ! Read the YAML file into a config object
  fileName = "species_database_test.yml"
  PRINT*, "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml, RC ) 

  ! FORMAT statements
10 FORMAT( a30, " | ", a    )
20 FORMAT( a30, " | ", L7   )
30 FORMAT( a30, " | ", i7   )
40 FORMAT( a30, " | ", f7.2 )

  species(1) = "ACET"
  species(2) = "ALD2"
  species(3) = "ALK4"
  species(4) = "ASOA1"

  ! Loop over the number of species
  DO S = 1, SIZE( species )

     ! Species name
     spc = species(S)

     ! Loop over the number of tags in the species database
     DO N = 1, SIZE( tags ) 
     
        ! Set intial values to missing values
        v_bool = MISSING_BOOL
        v_int  = MISSING_INT
        v_real = MISSING_REAL
        v_str  = MISSING_STR

        ! Search key
        key = TRIM( spc ) // '%' // TRIM( tags(N) )

        ! strings
        IF ( INDEX( key, "Fullname" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_str, "" )
           WRITE( 6, 10 ) TRIM( key ), TRIM( v_str )

        ELSE IF ( INDEX( key, "Formula" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_str, "" )
           WRITE( 6, 10 ) TRIM( key ), TRIM( v_str )          

        ! booleans
        ELSE IF ( INDEX( key, "Is_Advected" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_Aero" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_DryAlt" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_DryDep" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_HygroGrowth" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_Gas" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_Photolysis" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_WetDep" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_Hg0" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_Hg2" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "Is_HgP" ) > 0 ) THEN 
           CALL QFYAML_Add_Get( yml, key, v_bool, "" )
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE
           ! Pass

        ENDIF

     ENDDO

     print*, "###"
  ENDDO

  ! Finalize the config object
  print*, "### finishing"
  CALL QFYAML_CleanUp( yml )

END PROGRAM test_qfyaml
