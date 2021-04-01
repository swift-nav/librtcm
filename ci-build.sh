#!/bin/bash

# Run Travis setup

set -e
set -x
set -o errexit
set -o pipefail

function build_haskell () {
    cd haskell
    stack install --test
    cd ..
}

function build_c() {
    mkdir -p c/build
    cd c/build
    cmake .. -DCMAKE_BUILD_TYPE=Release
    make -j4
    cd ../..
}

if [ "$TESTENV" == "stack-macos" ] || [ "$TESTENV" == "stack-windows" ] || [ "$TESTENV" == "stack-ubuntu" ]; then
  build_haskell
else
  build_c
fi

if [[ "$TESTENV" == "stack-windows" ]]; then
  cp "$APPDATA/local/bin/rtcm32json.exe" "$APPDATA/local/bin/json2rtcm3.exe" .
  7z a -tzip librtcm_windows.zip rtcm32json.exe json2rtcm3.exe
  VERSION="$(git describe --always --tags)"
  BUILD_TRIPLET="windows-x86_64"
  mv librtcm_windows.zip "librtcm-${VERSION}-${BUILD_TRIPLET}.zip"
  echo "librtcm-${VERSION}-${BUILD_TRIPLET}.zip" >release-archive.filename
  ls -l
elif [[ "$TESTENV" == "stack-macos" ]] || [[ "$TESTENV" == "stack-ubuntu" ]]; then
  cp "$HOME"/.local/bin/{rtcm32json,json2rtcm3} .
  tar -czf librtcm.tar.gz rtcm32json json2rtcm3
  VERSION="$(git describe --always --tags)"
  if [[ "$TESTENV" == "stack-macos" ]]; then
    BUILD_TRIPLET="macos-x86_64"
  elif [[ "$TESTENV" == "stack-ubuntu" ]]; then
    BUILD_TRIPLET="linux-x86_64"
  fi
  mv librtcm.tar.gz "librtcm-${VERSION}-${BUILD_TRIPLET}.tar.gz"
  echo "librtcm-${VERSION}-${BUILD_TRIPLET}.tar.gz" >release-archive.filename
  ls -l
fi
