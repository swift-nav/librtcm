#!/bin/bash

# Run Travis setup

set -e
set -x
set -o errexit
set -o pipefail

function build_haskell () {
    cd haskell
    stack install --test
    cd ../
}

function build_c() {
    cd c
    mkdir build
    cd build
    cmake ../ -DCMAKE_BUILD_TYPE=Release
    make -j4
    cd ../
    cd ../
}

if [ "$TESTENV" == "stack-osx" ] || [ "$TESTENV" == "stack-windows" ] || [ "$TESTENV" == "stack-linux" ]; then
  build_haskell
else
  build_c
fi
