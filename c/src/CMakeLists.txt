cmake_minimum_required(VERSION 2.8.7)

file(GLOB librtcm_HEADERS "${PROJECT_SOURCE_DIR}/include/*.h")

add_library (librtcm rtcm3_decode.c)
target_link_libraries(librtcm m)
target_include_directories (librtcm PUBLIC ../include)

install(FILES ${libsbp_HEADERS} DESTINATION include)
