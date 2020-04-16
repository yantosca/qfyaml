# Compiler specific flags for GNU Fortran compiler
# NOTE: Flags are taken from the GEOS-Chem build sequence

# Base set of compiler flags
set(CMAKE_Fortran_FLAGS "-g -O0 -cpp -fbacktrace -ffree-line-length-512 -w -fautomatic -fno-align-commons -fno-range-check -mcmodel=medium -std=legacy")

# Additional compiler flags for Release
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Additional compiler flags for Debug
set(CMAKE_Fortran_FLAGS_DEBUG  "-O0 -gdwarf-2 -gstrict-dwarf -O0 -Wall -Wextra -Wconversion -Warray-temporaries -fcheck-array-temporaries")

# Set a cpp switch to denote GNU
add_definitions(-D_GNU)
