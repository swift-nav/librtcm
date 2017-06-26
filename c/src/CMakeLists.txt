cmake_minimum_required(VERSION 2.8.7)
project(librtcm)

add_library (librtcm rtcm3_decode.c)
target_link_libraries(librtcm m)
target_include_directories (librtcm PUBLIC ../include)
