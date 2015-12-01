cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
project("PlPlotLib")

set(sources_common
	"src/kinds.f90"
	"src/plplotlib.f90"
	"src/utilities.f90"
	)

set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -Wno-tabs -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=snan")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")

include(FindPkgConfig)
pkg_search_module(PLPLOT REQUIRED plplot-f95 plplotd-f95)
include_directories(${PLPLOT_INCLUDE_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})

add_executable(logo_prg ${sources_common} "src/logo.f90")
target_link_libraries(logo_prg ${PLPLOT_LIBRARIES})

add_executable(basic_prg ${sources_common} "src/basic.f90")
target_link_libraries(basic_prg ${PLPLOT_LIBRARIES})
add_custom_command(TARGET basic_prg PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/examples"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_executable(animate_prg ${sources_common} "src/animate.f90")
target_link_libraries(animate_prg ${PLPLOT_LIBRARIES})

add_executable(examples_prg ${sources_common} "src/examples.f90")
target_link_libraries(examples_prg ${PLPLOT_LIBRARIES})
add_custom_command(TARGET examples_prg PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/examples"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

set(FORD_EXECUTABLE "ford")
add_custom_target(ford
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/pages" "${CMAKE_BINARY_DIR}/pages"
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/src" "${CMAKE_BINARY_DIR}/src"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/project.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${FORD_EXECUTABLE} "project.md"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")


