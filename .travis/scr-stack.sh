#!/bin/bash

set -ev

time stack build
time stack test

