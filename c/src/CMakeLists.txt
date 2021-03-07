cmake_minimum_required(VERSION 2.8.7)

# todo: use bits.[ch] from libswiftnav, MAP-605
set(librtcm_HEADERS
  ${PROJECT_SOURCE_DIR}/include/rtcm3/bits.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/constants.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/messages.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/encode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/sta_decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/decode_macros.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/eph_decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/eph_encode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/ssr_decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/msm_utils.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/logging.h
  )

add_library(rtcm
  decode.c
  encode.c
  msm_utils.c
  eph_decode.c
  eph_encode.c
  ssr_decode.c
  sta_decode.c
  bits.c
  logging.c
  )
swift_set_language_standards(rtcm)
swift_set_compile_options(rtcm REMOVE -Wconversion -Wstack-protector)

target_include_directories(rtcm PUBLIC ${PROJECT_SOURCE_DIR}/include)

if (NOT CMAKE_C_COMPILER_ID MATCHES "ARMCC")
  target_link_libraries(rtcm m)
endif()

install(TARGETS rtcm DESTINATION ${CMAKE_INSTALL_FULL_LIBDIR})
install(FILES ${librtcm_HEADERS} DESTINATION ${CMAKE_INSTALL_FULL_INCLUDEDIR}/rtcm3)
