#### Specific flags for GNU Fortran compiler

# Base set of GNU compiler flags
set(CMAKE_Fortran_FLAGS_GNU
  -g -O0 -cpp -fbacktrace -ffree-line-length-512 -w -fautomatic
  -fno-align-commons -fno-range-check -mcmodel=medium -std=legacy
)

# Additional GNI compiler flags for Release
set(CMAKE_Fortran_FLAGS_RELEASE_GNU
  -O3
)

# Additional GNU compiler flags for Debug
set(CMAKE_Fortran_FLAGS_DEBUG_GNU
  -O0 -gdwarf-2 -gstrict-dwarf -O0 -Wall -Wextra -Wconversion
  -Warray-temporaries -fcheck-array-temporaries
)
