swift_add_test(test-rtcm
  UNIT_TEST
  POST_BUILD
  SRCS rtcm_decoder_tests.c
  LINK rtcm
  )
swift_set_language_standards(test-rtcm)
swift_set_compile_options(test-rtcm REMOVE -Wconversion ADD -Wno-discarded-qualifiers)

swift_add_test(test-sta
  UNIT_TEST
  POST_BUILD
  SRCS sta_rtcm3_tests.c
  LINK rtcm
  )
swift_set_language_standards(test-sta)
swift_set_compile_options(test-sta REMOVE -Wconversion)

