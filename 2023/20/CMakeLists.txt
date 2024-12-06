CMake_minimum_required(VERSION 3.15)

Project(advent_of_code Fortran)
include(FindPkgConfig)
pkg_search_module(StringiFor REQUIRED stringifor)
pkg_search_module(FTL REQUIRED ftl)

add_library(mod_lib mod_module.f90)
target_include_directories(mod_lib PRIVATE ${StringiFor_INCLUDE_DIRS})

add_library(ftl_hashmap ftlHashMapStringModModule.F90)
target_include_directories(ftl_hashmap PRIVATE ${FTL_INCLUDE_DIRS})

add_executable(main1 main1.f90)
target_include_directories(main1
   PUBLIC ${StringiFor_INCLUDE_DIRS} ${FTL_INCLUDE_DIRS}
   ${mod_lib_INCLUDE_DIRS} ${ftl_hashmap_INCLUDE_DIRS})
target_link_libraries(main1
   PRIVATE ${StringiFor_LINK_LIBRARIES} ${FTL_LINK_LIBRARIES}
   mod_lib ftl_hashmap)
