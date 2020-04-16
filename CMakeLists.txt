# Minimum required CMake version
cmake_minimum_required(VERSION 3.5)

# Print an error message if we don't have the required version
if(${CMAKE_VERSION} VERSION_LESS 3.5)
    cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
endif()

# Define project name, version, and language
project (
  qfyaml
  VERSION 0.1.0 
  LANGUAGES Fortran
)

# Most users of this software do not (should not?) have permissions to
# install in the cmake default of /usr/local (or equiv on other os's).
# Below, the default is changed to the bin subfolder, unless the user 
# explicitly sets CMAKE_INSTALL_PREFIX in the cache.
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}/bin" CACHE PATH "default install path" FORCE )
endif()

# Set the path where CMake will look the build_submodule.cmake script
# and the scripts GNU.cmake and Intel.make which specify compiler info.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")

# Make sure we have one of the recognized compilers
include(${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, or gfortran.")
endif()

# Compile code these folders
add_subdirectory(src)
