cmake_minimum_required(VERSION 2.8.7)

if (CMAKE_CROSSCOMPILING)
    message(STATUS "Skipping unit tests, cross compiling")
else (CMAKE_CROSSCOMPILING)
    add_executable (librtcm_test rtcm_decoder_tests.c rtcm_encoder.c)

    target_link_libraries(librtcm_test LINK_PUBLIC librtcm)

    add_custom_command(
        TARGET librtcm_test POST_BUILD
        COMMENT "Running unit tests"
        COMMAND librtcm_test
    )
endif(CMAKE_CROSSCOMPILING)
