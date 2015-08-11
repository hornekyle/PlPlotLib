cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
project("PlPlotLib")

set(sources
	"src/examples.f90"
	"src/kinds.f90"
	"src/main.f90"
	"src/plplotlib.f90"
	)

set(CMAKE_Fortran_FLAGS "-g -Wall -Wtabs")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -Wtabs -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=snan")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")

include(FindPkgConfig)
pkg_search_module(PLPLOT REQUIRED plplot-f95 plplotd-f95)
include_directories(${PLPLOT_INCLUDE_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})

add_executable(main ${sources})
target_link_libraries(main ${PLPLOT_LIBRARIES})

set(FORD_EXECUTABLE "ford")
add_custom_target(ford
	COMMAND ${FORD_EXECUTABLE} "${CMAKE_SOURCE_DIR}/project.md"
	WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}")
