#!/bin/bash

set -ev

stack build
stack test

