#### TEST_QFYAML EXAMPLE 

add_executable(test_qfyaml.x
  test_qfyaml.F90
  qfyaml_mod.F90
  precision_mod.F90
  roundoff_mod.F90
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
  precision_mod.F90
  roundoff_mod.F90
  qfyaml_mod.F90
  test_species_database.F90
)
install(
  TARGETS test_species_database.x
  RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}
)
install(
  FILES species_database_test.yml
  DESTINATION ${CMAKE_INSTALL_PREFIX}
)
