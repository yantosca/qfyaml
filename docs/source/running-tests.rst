.. _Running tests:

################################
Running the qfyaml test programs
################################

Several test programs are included with :program:`qfyaml`.  After
installation, executables for these tests will be placed in the
:file:`qfyaml/bin` folder.

.. _general_tests:

=====================
General test programs
=====================

The following test programs can be used to help debug issues with the
:program:`qfyaml` code itself.


test_qfyaml.x
-------------

This test reads a general YAML file (`input.yml
<https://github.com/yantosca/qfyaml/blob/main/test/input.yml>`_) that
is designed to catch several issues with parsing YAML markuup.

To run this test, type at the command line:

.. code-block:: console

   $ cd qfyaml/bin      # Skip if you are already in qfyaml/bin
   $ ./test_qfyaml.x

This test will parse the :file:`input.yml` file and echo back the
output.  If the test is successful you will see this output:

.. code-block:: console

   ### Reading qfyaml.yml

   ### YAML VARIABLES
   author%age                       :      29
   author%fav_reals                 :    1.00   2.00
   author%more_reals                :    3.141590   2.781280   8.573900 101.324997
   author%lots_of_work              :       F
   author_name%first                : Homer
   author_name%full                 : Homer J. Simpson
   filename                         : another_file
   weather%humidity                 :     99.858582
   weather%temperature%daily        :     23.043436
   weather%temperature%weekly%units : K
   weather%pressure                 :   1013.250000

   ### FIND NEXT-HIGHER VARIABLES IN "weather"
               1 weather%humidity
               2 weather%pressure
               3 weather%temperature

   ### YAML SEQUENCES
   fruits
              1 Apples
              2 Bananas
              3 Oranges

   more_fruits%p_fruits
              1 Pears
              2 Plums
              3 Peaches
              4 Pomegranites

   even_more_fruits%exotic_fruits%hard_to_find
              1 Kumquats
              2 Kiwi
              3 Passion_fruit
              4 Star_fruit
              5 Durians

   #### finishing

.. _geos-chem_tests:

================================
GEOS-Chem-specific test programs
================================

The following test programs can be used to debug source code for
reading YAML-format configuration files into the `GEOS-Chem model
<https://geos-chem.org>`_.

.. _test_config.x:

test_config.x
-------------

This test program attempts to read the `GEOS-Chem master configuration
file
<https://github.com/yantosca/qfyaml/blob/main/test/geoschem_config.yml>`_
and echo back output.  The master configuration file will replace
:file:`input.geos` in GEOS-Chem 14.0.0 and later.

To run this test, type at the command line:

.. code-block:: console

   $ cd qfyaml/bin      # Skip if you are already in qfyaml/bin
   $ ./test_config.x

And you should see output such as:

.. code-block :: console

   ### Reading input_options.yml
    simulation%start
    ==>     20190701           0
    simulation%end
    ==>     20190801           0
    simulation%data_dir
    ==> /n/holyscratch01/external_repos/GEOS-CHEM/gcgrid/data/ExtData
    simulation%met_field
    ==> MERRA2
    simulation%name
    ==> fullchem
    simulation%species_database_file
    ==> species_database.yml
    simulation%debug_printout
    ==>  F
    simulation%use_gcclassic_timers
    ==>  F

    grid%resolution
    ==> 0.5x0.625
    grid%longitude_range
    ==>   -140.000000      -40.0000000
    grid%center_lon_at_180
    ==>  T
    grid%latitude_range
    ==>   -10.0000000       70.0000000
    grid%half_size_polar_boxes
    ==>  T
    grid%number_of_levels
    ==>           72
    grid%nested_grid_simulation
    ==>  T
    grid%buffer_zone_NSEW
    ==>            3           3           3           3

    . . . etc . . .

.. _test_species_database.x:

test_species_database.x
-----------------------

This test program attemps to read the `GEOS-Chem species database
<https://github.com/yantosca/qfyaml/blob/main/test/species_database.yml>`_
file and echo back output.

To run this test, type at the command line:

.. code-block:: console

   $ cd qfyaml/bin      # Skip if you are already in qfyaml/bin
   $ ./test_species_database.x

You should see output similar to this:

.. code-block:: console

    ### Reading species_database.yml
    ### Reading species_database_tomas.yml
               ACTA%Background_VV |    -999.00
               ACTA%DD_AeroDryDep |          F
               ACTA%DD_DustDryDep |          F
               ACTA%DD_DvzAerSnow |    -999.00
                ACTA%DD_DvzMinVal |    -999.00   -999.00
                       ACTA%DD_F0 |       1.00
                    ACTA%DD_Hstar |    4100.00
                     ACTA%Density |    -999.00
                     ACTA%Formula | CH3C(O)OH
                    ACTA%FullName | Acetic acid
                 ACTA%Is_Advected |          T
                  ACTA%Is_Aerosol |          F
                   ACTA%Is_DryAlt |          F
                   ACTA%Is_DryDep |          T
              ACTA%Is_HygroGrowth |          F
                      ACTA%Is_Gas |          T
                      ACTA%Is_Hg0 |          F
                      ACTA%Is_Hg2 |          F
                      ACTA%Is_HgP |          F
               ACTA%Is_Photolysis |          F
                   ACTA%Is_WetDep |          T
                    ACTA%Henry_CR |    6200.00
                    ACTA%Henry_K0 |    4050.00
                   ACTA%Henry_pKa |    -999.00
               ACTA%MP_SizeResAer |          F
               ACTA%MP_SizeResNum |          F
                  ACTA%MolecRatio |       1.00
                        ACTA%MW_g |      60.06
               ACTA%WD_AerScavEff |    -999.00
                ACTA%WD_CoarseAer |          F
               ACTA%WD_ConvFacI2G |    -999.00
               ACTA%WD_KcScaleFac |    -999.00   -999.00   -999.00
           ACTA%WD_KcScaleFac_Luo |    -999.00   -999.00   -999.00
                ACTA%WD_LiqAndGas |          F
               ACTA%WD_RainoutEff |    -999.00   -999.00   -999.00
           ACTA%WD_RainoutEff_Luo |    -999.00   -999.00   -999.00
                ACTA%WD_RetFactor |       0.02

   . . . etc . . .
