cmake_minimum_required(VERSION 3.3)
project(librtcm C CXX)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake" "${CMAKE_CURRENT_LIST_DIR}/cmake/common")

include(GNUInstallDirs)
include(CCache)
include(SwiftCmakeOptions)
include(LanguageStandards)
include(CompileOptions)
include(CheckAttributes)
create_check_attributes_target()
include(SwiftTargets)
include(CodeCoverage)
add_code_coverage_all_targets()

swift_create_project_options(
    HAS_TESTS
    )

include(ClangFormat)
swift_setup_clang_format()

include(ClangTidy)

include(TestTargets)

find_package(Swiftnav REQUIRED)

add_subdirectory (src)

if(librtcm_BUILD_TESTS)
  add_subdirectory (test)
endif()

swift_validate_targets()
swift_create_clang_tidy_targets()
