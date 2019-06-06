add_executable (rtcm_test rtcm_decoder_tests.c)

target_link_libraries(rtcm_test rtcm)

add_custom_command(
    TARGET rtcm_test POST_BUILD
    COMMENT "Running unit tests"
    COMMAND rtcm_test
)
set_target_properties(rtcm_test PROPERTIES C_STANDARD 99)

