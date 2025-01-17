file (GLOB_RECURSE FORTRAN_SOURCES *.f90)

add_executable(fortran_meets_sdl ${FORTRAN_SOURCES})
target_link_libraries(fortran_meets_sdl PRIVATE SDL3::SDL3)