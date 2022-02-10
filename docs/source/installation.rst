.. _Installation:

############################
Installation and compilation
############################

.. _Downloading:

******************
Downloading qfyaml
******************

Use Git to clone the source code from the `qfyaml repository
<https://github.com/yantosca/qfyaml>`_  to your computer system:

.. code-block:: console

   $ git clone https://github.com/yantosca/qfyaml.git

This will create a folder named :file:`qfyaml` in your disk space.
   
The main :program:`qfyaml` source code file is
:file:`qfyaml/src/qfyaml_mod.F90`.  Several test programs are included
in the :file:`qfyaml/test` folder.

.. _Configuring:

******************
Configuring qfyaml
******************

You may now call CMake, which will begin the configuration
process.  During configuration, CMake will first check if you
have everything on your system that is required to compile the
:program:`qfyaml` source code.  If everything checks out, then Cmake
will create several Makefiles to be used during compilation.

Navigate to the :file:`qfyaml/bin` folder:

.. code-block:: console

   $ cd qfyaml/bin

and call CMake with this command:

.. code-block:: console

   $ cmake .. -B ../build

The :file:`-B ../build` command tells CMake to create the Makefiles in
the :file:`qfyaml/build` folder.
   
You should see output similar to this:

.. code-block:: console

   -- The Fortran compiler identification is GNU 11.1.0
   -- Detecting Fortran compiler ABI info
   -- Detecting Fortran compiler ABI info - done
   -- Check for working Fortran compiler: /bin/gfortran - skipped
   -- Configuring done
   -- Generating done
   -- Build files have been written to: /home/bob/work/qfyaml/build

.. _Compiling:
   
***************
Compling qfyaml
***************

Once the configuration step has completed successfully, you may
compile the :program:`qfyaml` source code.  

Type the following at the command line:

.. code-block:: console

   $ make -C ../build

The :file:`-C` command tells :program:`make` to look in the
:file:`../build` folder for the Makefiles that were created by CMake.
These Makefiles will direct the build process.

You should see output similar to this.

.. code-block:: console

   make: Entering directory '/home/bob/work/qfyaml/build'
   make[1]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   Scanning dependencies of target QfYaml
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   [  9%] Building Fortran object src/CMakeFiles/QfYaml.dir/qfyaml_mod.F90.o
   [ 18%] Linking Fortran static library libQfYaml.a
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 18%] Built target QfYaml
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   Scanning dependencies of target Common
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   [ 27%] Building Fortran object test/CMakeFiles/Common.dir/precision_mod.F90.o
   [ 36%] Building Fortran object test/CMakeFiles/Common.dir/roundoff_mod.F90.o
   [ 45%] Linking Fortran static library libCommon.a
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 45%] Built target Common
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   Scanning dependencies of target test_qfyaml.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   [ 54%] Building Fortran object test/CMakeFiles/test_qfyaml.x.dir/test_qfyaml.F90.o
   [ 63%] Linking Fortran executable test_qfyaml.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 63%] Built target test_qfyaml.x
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   Scanning dependencies of target test_species_database.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   [ 72%] Building Fortran object test/CMakeFiles/test_species_database.x.dir/test_species_database.F90.o
   [ 81%] Linking Fortran executable test_species_database.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 81%] Built target test_species_database.x
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   Scanning dependencies of target test_geoschem_config.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   [ 90%] Building Fortran object test/CMakeFiles/test_geoschem_config.x.dir/test_geoschem_config.F90.o
   [100%] Linking Fortran executable test_geoschem_config.x
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [100%] Built target test_geoschem_config.x
   make[1]: Leaving directory '/home/bob/work/qfyaml/build'
   make: Leaving directory '/home/bob/work/qfyaml/build'

.. _Installing:

*****************
Installing qfyaml
*****************

Once compilation has finished successfully, we may install the
compiled code (and various input files)  to the :file:`qfyaml/bin`
folder.

Type at the command line:

.. code-block:: console

   $ make -C ../build install

You should see output similar to this:

.. code-block:: console

   make: Entering directory '/home/bob/work/qfyaml/build'
   make[1]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 18%] Built target QfYaml
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 45%] Built target Common
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 63%] Built target test_qfyaml.x
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [ 81%] Built target test_species_database.x
   make[2]: Entering directory '/home/bob/work/qfyaml/build'
   make[2]: Leaving directory '/home/bob/work/qfyaml/build'
   [100%] Built target test_geoschem_config.x
   make[1]: Leaving directory '/home/bob/work/qfyaml/build'
   Install the project...
   -- Install configuration: ""
   -- Installing: /home/bob/work/qfyaml/bin/test_qfyaml.x
   -- Up-to-date: /home/bob/work/qfyaml/bin/input.yml
   -- Installing: /home/bob/work/qfyaml/bin/test_species_database.x
   -- Up-to-date: /home/bob/work/qfyaml/bin/species_database.yml
   -- Up-to-date: /home/bob/work/qfyaml/bin/species_database_apm.yml
   -- Up-to-date: /home/bob/work/qfyaml/bin/species_database_tomas.yml
   -- Installing: /home/bob/work/qfyaml/bin/test_geoschem_config.x
   -- Up-to-date: /home/bob/work/qfyaml/bin/geoschem_config.yml
   make: Leaving directory '/home/bob/work/qfyaml/build'

Then to see the files that were installed, type:

.. code-block:: console

   $ cd ../bin
   $ ls -1

and you will see this directory listing:

.. code-block:: console

   geoschem_config.yml
   input.yml
   species_database_apm.yml
   species_database_tomas.yml
   species_database.yml
   test_geoschem_config.x
   test_qfyaml.x
   test_species_database.x

Executable files for the various tests (which are discussed in the
next chapter) end with the :file:`.x` extension.  The corresponding
configuration files in YAML format end with the :file:`.yml` format.



