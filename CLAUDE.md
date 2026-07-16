# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

qfyaml ("quick and dirty" Fortran YAML parser) is a small, self-contained Fortran library, derived from
`config_fortran`, used by GEOS-Chem to read YAML configuration files. There is no external YAML/Fortran
dependency — the entire parser lives in one module, `src/qfyaml_mod.F90`.

## Build, install, and run tests

There is no `ctest`/`BUILD_TESTING` integration. Build from the `bin/` directory:

```bash
cd bin
cmake .. -B ../build                      # configure (add -DCMAKE_BUILD_TYPE=Debug|Release|RelWithDebInfo)
make -C ../build                          # compile
make -C ../build install                  # installs test_*.x executables + .yml fixtures into bin/
```

Requires a GNU (`gfortran`) or Intel (`ifort`) Fortran compiler — the root `CMakeLists.txt` errors out on
any other `CMAKE_Fortran_COMPILER_ID`. Compiler flags per build type live in `cmake/GNU.cmake` /
`cmake/Intel.cmake`.

Clean all build/install artifacts:

```bash
cd bin && ./cleanup.sh   # rm -f *.yml *.x; rm -rf ../build/*
```

**"Tests" are demo programs, not asserted unit tests.** Each `test_*.x` binary reads a `.yml` fixture and
prints parsed values to stdout for manual comparison against the sample output documented in
`docs/source/running-tests.rst` — there are no per-field assertions or pass/fail exit codes (beyond an
occasional `IF (RC /= QFYAML_Success) STOP` on a read error). Run an individual test from `bin/` after
installing, e.g. `./test_qfyaml.x`.

To add a new test: create `test/<name>.F90`, then add matching `add_executable`,
`target_link_libraries(... PUBLIC Common QfYaml BuildProperties)`, and `install(TARGETS ...)` /
`install(FILES <fixture>.yml ...)` blocks to `test/CMakeLists.txt`, mirroring the existing three targets
(`test_qfyaml.x`, `test_config.x`, `test_species_database.x`). Note `test/test_input_options.F90` currently
exists with no CMake target — it is orphaned/unbuilt, not a template to copy blindly.

## Architecture

Everything public lives in one module, `QFYAML_Mod` (`src/qfyaml_mod.F90`, ~3800 lines), written in the
classic GEOS-Chem "object-based" Fortran 95 style: derived types plus free procedures that take the type
as the first argument (not `TYPE ... CONTAINS` bound methods).

- **`QFYAML_t`** — the opaque config object. Holds a sorted, allocatable array of `QFYAML_var_t` entries
  (category, name, type, raw string value, and typed real/int/char/bool arrays, plus YAML anchor bookkeeping).
- **Lifecycle**: `QFYAML_Init(fileName, yml, yml_anchored, RC)` parses a file into a `QFYAML_t` (and a
  second one for anchor/alias targets) and sorts entries for binary search. Every `QFYAML_Init` must be
  paired with an explicit `QFYAML_CleanUp(yml)` on *both* objects — there is no destructor.
- **Primary read API**: `QFYAML_Add_Get` (generic over real/int/string/bool, scalar/array) — the caller
  passes a variable pre-set to its default value; the call returns either that default or the value found
  in the YAML file. This "declare default + read" pattern is what nearly all test code uses. Related
  generics: `QFYAML_Add`, `QFYAML_Get`, `QFYAML_Update`, `QFYAML_Merge` (concatenate two config objects),
  `QFYAML_Print` (serialize back to YAML text, optionally filtered by `searchKeys`), `QFYAML_FindNextHigher`
  (enumerate child keys one level below a `%`-joined category prefix).
- **Parsing engine** (private): `QFYAML_Read_File` drives a hand-rolled indentation-tracking state machine
  (`Parse_Line`) that builds `%`-joined category-qualified variable names (e.g. `weather%temperature%daily`),
  handling YAML sequences and anchors/aliases (`&anchor`, `*anchor`, `<<:`). Values are stored as raw
  strings first and typed lazily once the caller's `Add_Get_*` call reveals the expected type.
- **Error handling convention**: every public routine initializes `RC = QFYAML_Success`, and on failure
  calls `Handle_Error(errMsg, RC, thisLoc)` with a `thisLoc` string like
  `' -> at QFYAML_Init (in module qfyaml_mod.F90)'`. Follow this pattern for any new public routine.

## Known inconsistencies (don't "fix" without being asked — just be aware)

- `README.md`'s license badge says MIT; the actual license (`LICENSE.txt`, `.zenodo.json`) is GPL-3.0.
- `docs/source/running-tests.rst` references a `test_geoschem_config.x` binary; the current CMake target
  name is `test_config.x`.
- `cmake/build_submodule.cmake` defines a `build_submodule()` macro that is never invoked — there is no git
  submodule dependency (confirmed: no `.gitmodules`, `git submodule status` is empty).
