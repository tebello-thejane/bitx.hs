#!/bin/bash

set -ev

travis_retry sudo add-apt-repository -y ppa:hvr/ghc
travis_retry sudo apt-get update
time travis_retry sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER
export PATH=/home/travis/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
travis_retry cabal update

time cabal install --only-dependencies --enable-tests

