#### COMMON FILES FOR TEST PROGRAMS

add_library(Common
  STATIC EXCLUDE_FROM_ALL
  precision_mod.F90
  roundoff_mod.F90
)
target_link_libraries(Common
  PUBLIC
  BuildProperties
)

#### TEST_QFYAML EXAMPLE

add_executable(test_qfyaml.x
  test_qfyaml.F90
)
target_link_libraries(test_qfyaml.x
  PUBLIC
  Common
  QfYaml
  BuildProperties
)
install(
  TARGETS test_qfyaml.x
  RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}
)  
install(
  FILES input.yml
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)


### SPECIES DATABASE EXAMPLE

add_executable(test_species_database.x
  test_species_database.F90
)
target_link_libraries(test_species_database.x
  PUBLIC
  Common
  QfYaml
  BuildProperties
)
install(
  TARGETS test_species_database.x
  RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}
)
install(
  FILES species_database.yml
  FILES species_database_apm.yml
  FILES species_database_tomas.yml
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)


### GEOSCHEM CONFIG EXAMPLE

add_executable(test_geoschem_config.x
  test_geoschem_config.F90
)
target_link_libraries(test_geoschem_config.x
  PUBLIC
  Common
  QfYaml
  BuildProperties
)
install(
  TARGETS test_geoschem_config.x
  RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}
)
install(
  FILES geoschem_config.yml
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)
