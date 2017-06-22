cmake_minimum_required(VERSION 2.8.7)
project(librtcm)

add_executable (librtcm_test rtcm_decoder_tests.c)

target_link_libraries(librtcm_test LINK_PUBLIC librtcm)

add_custom_command(
        TARGET librtcm_test POST_BUILD
        COMMENT "Running unit tests"
        COMMAND librtcm_test
)
