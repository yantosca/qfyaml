#### TEST_QFYAML EXAMPLE 

add_library(QfYaml
  STATIC EXCLUDE_FROM_ALL
  qfyaml_mod.F90
)
target_link_libraries(QfYaml
  PUBLIC
  BuildProperties
)
