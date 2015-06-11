#!/bin/bash

set -ev

cabal configure --enable-tests -O0 --disable-library-profiling -v2
cabal build
cabal configure --enable-tests --enable-benchmarks -O0 --disable-library-profiling -v2
cabal test

