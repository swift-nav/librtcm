cmake_minimum_required(VERSION 2.8.7)

# todo: use bits.[ch] from libswiftnav, MAP-605
set(librtcm_HEADERS
  ${PROJECT_SOURCE_DIR}/include/rtcm3/bits.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/constants.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/messages.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/encode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/eph_decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/ssr_decode.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/msm_utils.h
  ${PROJECT_SOURCE_DIR}/include/rtcm3/logging.h
  )

add_library(rtcm
  decode.c
  encode.c
  msm_utils.c
  eph_decode.c
  ssr_decode.c
  bits.c
  logging.c
  )

target_link_libraries(rtcm m)
target_include_directories(rtcm PUBLIC ${PROJECT_SOURCE_DIR}/include)

target_compile_options(rtcm PRIVATE "-Wall")
target_compile_options(rtcm PRIVATE "-Wextra")
target_compile_options(rtcm PRIVATE "-Werror")
target_compile_options(rtcm PRIVATE "-Wimplicit")
target_compile_options(rtcm PRIVATE "-Wshadow")
target_compile_options(rtcm PRIVATE "-Wswitch-default")
target_compile_options(rtcm PRIVATE "-Wswitch-enum")
target_compile_options(rtcm PRIVATE "-Wundef")
target_compile_options(rtcm PRIVATE "-Wuninitialized")
target_compile_options(rtcm PRIVATE "-Wpointer-arith")
target_compile_options(rtcm PRIVATE "-Wcast-align")
target_compile_options(rtcm PRIVATE "-Wformat=2")
target_compile_options(rtcm PRIVATE "-Wimplicit-function-declaration")
target_compile_options(rtcm PRIVATE "-Wredundant-decls")
target_compile_options(rtcm PRIVATE "-Wformat-security")
target_compile_options(rtcm PRIVATE "-fno-unwind-tables")
target_compile_options(rtcm PRIVATE "-fno-asynchronous-unwind-tables")
target_compile_options(rtcm PRIVATE "-std=gnu99")
target_compile_options(rtcm PRIVATE "-fPIC")

install(TARGETS rtcm DESTINATION lib${LIB_SUFFIX})
install(FILES ${librtcm_HEADERS} DESTINATION include/rtcm3)
