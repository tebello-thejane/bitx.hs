#!/bin/bash

stack build &
pidA=$!

while true; do sleep 60; echo -e "\033[0;32m1 minute elapsed.\033[0m"; done &
pidB=$!

wait $pidA

echo -e "\033[0;32mstack build is finished.\033[0m"

kill -9 $pidB

