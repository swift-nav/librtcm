swift_add_test(test-rtcm
  POST_BUILD
  SRCS rtcm_decoder_tests.c
  LINK rtcm
  )

swift_add_test(test-sta
  POST_BUILD
  SRCS sta_rtcm3_tests.c
  LINK rtcm
  )
