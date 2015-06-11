#!/bin/bash

set -ev

sudo apt-get install libgmp3-dev git autoconf automake libtool make libgmp-dev ncurses-dev g++ python bzip2

time stack build
time stack test

