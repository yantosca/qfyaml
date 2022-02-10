##### Specific flags for Intel Fortran compiler

if(WIN32)
  set(no_optimize "-Od")
  set(check_all "-check:all")
else()
  set(no_optimize "-O0")
  set(check_all "-check all")
endif()


# Base set of Intel compiler flags
set(CMAKE_Fortran_FLAGS_Intel
  -cpp -w -auto -noalign -fp-model-source
  -mcmodel=medium -shared-intel -traceback ${no_optimize}
)

# Addtional Intel compiler flags for release
set(CMAKE_Fortran_FLAGS_RELEASE_Intel
  -O2
)

# Additional Intel compiler flags for debug
set(CMAKE_Fortran_FLAGS_Intel_DEBUG
  -g ${no_optimize} ${check_all} -fpe0 -ftrapuv
)
