!------------------------------------------------------------------------------
!                  GEOS-Chem Global Chemical Transport Model                  !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Test_Species_Database
!
! !DESCRIPTION: Test program for reading the GEOS-Chem species database
!  in YAML format.
!\\
!\\
! !INTERFACE:
!
PROGRAM Test_Species_Database
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
  REAL(yp)                    :: mw_g

  ! Strings
  CHARACTER(LEN=14)           :: tag
  CHARACTER(LEN=31)           :: spc
  CHARACTER(LEN=255)          :: v_str
  CHARACTER(LEN=255)          :: key
  CHARACTER(LEN=255)          :: fileName

  ! Arrays
  REAL(yp)                    :: a_real_2(2)
  REAL(yp)                    :: a_real_3(3)

  ! String arrays
  CHARACTER(LEN=17)           :: tags(46)
  CHARACTER(LEN=31)           :: species(11)

  ! Objects
  TYPE(QFYAML_t)              :: yml
  TYPE(QFYAML_t)              :: yml1
  TYPE(QFYAML_t)              :: yml2
  TYPE(QFYAML_t)              :: yml_anchored
  TYPE(QFYAML_t)              :: yml1_anchored
  TYPE(QFYAML_t)              :: yml2_anchored
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
  !=========================================================================
  ! Test_QFYAML begins here!
  !=========================================================================

  RC         = QFYAML_SUCCESS
  mw_g       = MISSING_INT
  species(1) = "CO"
  species(2) = "COAnthroEmis25dayTracer"
  species(3) = "ALK4"
  species(4) = "ASOA1"
  species(5) = "ASOA2"
  species(6) = "Be7"
  species(7) = "Be10"
  species(8) = "Be7Strat"
  species(9) = "Be10Strat"
  species(10) = "AW1"
  species(11) = "AW2"

  ! Species database tags to match
  tags = (/ "Background_VV    ", "DD_AeroDryDep    ", "DD_DustDryDep    ",   &
            "DD_DvzAerSnow    ", "DD_DvzMinVal     ", "DD_F0            ",   &
            "DD_Hstar         ", "DD_KOA           ", "Density          ",   &
            "Formula          ", "FullName         ", "Is_ActiveChem    ",   &
            "Is_Advected      ", "Is_Aerosol       ", "Is_DryAlt        ",   &
            "Is_DryDep        ", "Is_FixedChem     ", "Is_HygroGrowth   ",   &
            "Is_Kpp           ", "Is_Gas           ", "Is_Hg0           ",   &
            "Is_Hg2           ", "Is_HgP           ", "Is_Photolysis    ",   &
            "Is_WetDep        ", "Henry_CR         ", "Henry_K0         ",   &
            "Henry_pKa        ", "MP_SizeResAer    ", "MP_SizeResNum    ",   &
            "MolecRatio       ", "MW_g             ", "EmMw_g           ",   &
            "Radius           ", "WD_AerScavEff    ", "WD_CoarseAer     ",   &
            "WD_ConvFacI2G    ", "WD_KcScaleFac    ", "WD_KcScaleFac_Luo",   &
            "WD_Is_H2SO4      ", "WD_Is_HNO3       ", "WD_Is_SO2        ",   &
            "WD_LiqAndGas     ", "WD_RainoutEff    ", "WD_RainoutEff_Luo",   &
            "WD_RetFactor     "/)

  ! Read the YAML file into a config object
  fileName = "species_database.yml"
  PRINT*, "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml1, yml1_anchored, RC )
  IF ( RC /= QFYAML_Success ) STOP

  ! Read another YAML file into another config object
  fileName = "species_database_tomas.yml"
  PRINT*, "### Reading " // TRIM( fileName )
  CALL QFYAML_Init( fileName, yml2, yml2_anchored, RC )
  IF ( RC /= QFYAML_Success ) STOP

  ! Merge two YAML objects into a single object
  CALL QFYAML_Merge( yml1, yml2, yml, RC )

  ! FORMAT statements
10 FORMAT( a30, " | ", a      )
20 FORMAT( a30, " | ", L10    )
30 FORMAT( a30, " | ", f10.2  )
31 FORMAT( a30, " | ", 2f10.2 )
32 FORMAT( a30, " | ", 3f10.2 )
40 FORMAT( a30, " | ", i10    )

  ! Loop over the number of species
  DO S = 1, SIZE( species )

     ! Species name
     spc = species(S)

     ! Loop over the number of tags in the species database
     DO N = 1, SIZE( tags )

        ! Set intial values to default "missing" values
        ! This will force creation of variables with these values
        a_real_2 = MISSING_REAL
        a_real_3 = MISSING_REAL
        v_bool   = MISSING_BOOL
        v_int    = MISSING_INT
        v_real   = MISSING_REAL
        v_str    = MISSING_STR

        ! Search key
        key = TRIM( spc ) // '%' // TRIM( tags(N) )

        ! Save into the proper field of the species database
        IF ( INDEX( key, "%Background_VV" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%DD_AeroDryDep" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%DD_DustDryDep" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%DD_DvzAerSnow" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%DD_DvzMinVal" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, a_real_2, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 31 ) TRIM( key ), a_real_2

        ELSE IF ( INDEX( key, "%DD_F0" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%DD_Hstar" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%Density" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%Formula" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 10 ) TRIM( key ), TRIM( v_str )

        ELSE IF ( INDEX( key, "%FullName" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_str, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 10 ) TRIM( key ), TRIM( v_str )

        ELSE IF ( INDEX( key, "%Is_Advected" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_Aero" ) > 0  ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_DryAlt" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_DryDep" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_HygroGrowth" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_Gas" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_Photolysis" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_WetDep" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_Hg0" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_Hg2" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Is_HgP" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%Henry_K0" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%Henry_CR" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%Henry_pKa" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%MP_SizeResAer" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%MP_SizeResNum" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%MolecRatio" ) > 0 ) THEN
           v_real = ONE                                  ! Set default to 1
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%MW_g" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real
           mw_g = v_real                                 ! Default for EmMw_g

        ELSE IF ( INDEX( key, "%EmMW_g" ) > 0 ) THEN
           v_real = mw_g
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real
           mw_g = MISSING_REAL                           ! Reset for next spc

        ELSE IF ( INDEX( key, "%DD_Radius" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%WD_AerScavEff" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%WD_CoarseAer" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%WD_ConvFacI2G" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE IF ( INDEX( key, "%WD_KcScaleFac" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 32 ) TRIM( key ), a_real_3

        ELSE IF ( INDEX( key, "%WD_KcScaleFac_Luo" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 32 ) TRIM( key ), a_real_3

        ELSE IF ( INDEX( key, "%WD_LiqAndGas" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_bool, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 20 ) TRIM( key ), v_bool

        ELSE IF ( INDEX( key, "%WD_RainoutEff" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 32 ) TRIM( key ), a_real_3

        ELSE IF ( INDEX( key, "%WD_RainoutEff_Luo" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, a_real_3, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 32 ) TRIM( key ), a_real_3

        ELSE IF ( INDEX( key, "%WD_RetFactor" ) > 0 ) THEN
           CALL QFYAML_Add_Get( yml, key, v_real, "", RC )
           IF ( RC /= QFYAML_Success ) GOTO 999
           WRITE( 6, 30 ) TRIM( key ), v_real

        ELSE
           ! Pass

        ENDIF

     ENDDO
  ENDDO

  !=========================================================================
  ! Print metadata output for only the species requested
  !=========================================================================
  print*, "### Writing requested species to species_output.yml"
  CALL QFYAML_Print( yml        = yml,                                      &
                     RC         = RC,                                       &
                     fileName   = 'species_output.yml',                     &
                     searchKeys = species                                  )

  IF ( RC /= QFYAML_SUCCESS ) THEN
     WRITE( 6, '(a)' ) 'Could not write YAML output file!'
  ENDIF

  !=========================================================================
  ! Print metadata output for only the species requested
  !=========================================================================
  print*, "### Writing requested species to stdout"
  CALL QFYAML_Print( yml        = yml,                                      &
                     RC         = RC,                                       &
                     fileName   = '*',                                      &
                     searchKeys = species                                  )

  IF ( RC /= QFYAML_SUCCESS ) THEN
     WRITE( 6, '(a)' ) 'Could not write YAML output file!'
  ENDIF

  !=========================================================================
  ! Finalize the config objects
  !=========================================================================
999 CONTINUE
  print*, "### finishing"
  CALL QFYAML_CleanUp( yml1          )
  CALL QFYAML_CleanUp( yml1_anchored )
  CALL QFYAML_CleanUp( yml2          )
  CALL QFYAML_CleanUp( yml2_anchored )
  CALL QFYAML_CleanUp( yml           )

END PROGRAM Test_Species_Database
