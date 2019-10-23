swift_add_test(test-rtcm
  POST_BUILD
  SRCS rtcm_decoder_tests.c
  LINK rtcm
  )

