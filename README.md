[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/yantosca/yaml_test/blob/master/LICENSE.txt)

# qfyaml: Quick Fortran YAML parserl

This repository contains a quick and dirty YAML parser written in Fortran. It is built off the existing package https://github.com/jannisteunissen/config_fortran.

Contact: yantosca [at] seas.harvard.edu 

## Installation

Clone the repository to your disk space:
```
git clone https://github.com/yantosca/qfyaml.git
```
## Compilation
Make sure you are in the bin subdirectory of yaml_test, e.g.
```
cd qfyaml/bin
```
Then run CMake to configure the build.  If you wish to compile with debugging flags, then type:
```
cmake -S ../ -B ../build -DCMAKE_BUILD_TYPE=Debug
```
otherwise, type:
```
cmake -S ../ -B ../build 
```
which will omit the debugging flags.  (For development we always recommend using debugging.)

To compile and install the executable files, type:
```
make -C ../build install
```
By default, this will create in the bin folder two example executables `test_qfyaml.x` and `test_species_database.x`, along with the relevant YAML files.  You can then run either of these by typing:
```
./test_qfyaml.x
```
or 
```
./test_species_database.x
```
You can modify these as needed, or add extra programs to the src folder.  You will have to modify the CMakeLists accordingly in that case.
