cmake_minimum_required(VERSION 3.5)

project(fortress)
enable_language(Fortran)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none -fno-range-check")
    if(MINGW)
      set(dialect "${dialect} -static-libgfortran")
    endif(MINGW)
    set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand f08 -free -implicitnone")
    set(bounds "-check bounds")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

#
# Compile.
#
file(GLOB_RECURSE sources  src/*.f90 src/*.f03 src/*.f08 src/*.h)
add_executable(fortress ${sources})
