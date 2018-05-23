cmake_minimum_required(VERSION 2.8.7)

file(GLOB librtcm_HEADERS "${PROJECT_SOURCE_DIR}/include/*.h")

add_library (rtcm rtcm3_decode.c rtcm3_msm_utils.c bits.c)
target_link_libraries(rtcm m)
target_include_directories (rtcm PUBLIC ../include)

install(TARGETS rtcm DESTINATION lib${LIB_SUFFIX})
install(FILES ${librtcm_HEADERS} DESTINATION include)
