file (GLOB_RECURSE FORTRAN_SOURCES *.f90)

set(GAME_NAME "space_invaders")

add_executable(${GAME_NAME} ${FORTRAN_SOURCES})
target_link_libraries(${GAME_NAME} PRIVATE SDL3::SDL3)