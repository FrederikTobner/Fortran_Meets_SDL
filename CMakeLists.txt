cmake_minimum_required(VERSION 3.24)

project(FortranMeetsSDL
        VERSION 0.1
        DESCRIPTION "A simple Fortran program that uses SDL"
        LANGUAGES Fortran)

# Add the external cmake files from my github repository
include(FetchContent)
FetchContent_Declare(
  ExternalCMakeFiles
  GIT_REPOSITORY https://github.com/FrederikTobner/CMakeLib.git
  GIT_TAG main
  SOURCE_DIR ${CMAKE_BINARY_DIR}/external/ExternalCMakeFiles/src
)
FetchContent_MakeAvailable(ExternalCMakeFiles)
list(APPEND CMAKE_MODULE_PATH 
"${CMAKE_BINARY_DIR}/external/ExternalCMakeFiles/src/")
message(STATUS "CMAKE_MODULE_PATH: ${CMAKE_MODULE_PATH}")

include(AddDependency)

add_github_dependency_by_tag(SDL libsdl-org/SDL preview-3.1.8)

add_subdirectory(src)
