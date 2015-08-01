#!/bin/bash

set -ev

cabal --version
ghc --version

cabal configure --enable-tests -O0 --disable-library-profiling -v2
cabal build
cabal configure --enable-tests -O0 --disable-library-profiling -v2
cabal clean
rm ~/.cabal/setup-exe-cache/*
cabal test

cabal check
cabal sdist
cabal install --force-reinstalls dist/*-*.tar.gz
