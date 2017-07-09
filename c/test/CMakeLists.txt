cmake_minimum_required(VERSION 2.8.7)

if (CMAKE_CROSSCOMPILING)
    message(STATUS "Skipping unit tests, cross compiling")
else (CMAKE_CROSSCOMPILING)
    add_executable (rtcm_test rtcm_decoder_tests.c rtcm_encoder.c)

    target_link_libraries(rtcm_test LINK_PUBLIC rtcm)

    add_custom_command(
        TARGET rtcm_test POST_BUILD
        COMMENT "Running unit tests"
        COMMAND rtcm_test
    )
endif(CMAKE_CROSSCOMPILING)
