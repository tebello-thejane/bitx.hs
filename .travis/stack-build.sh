#!/bin/bash

(sudo stack setup; sudo stack build) &
pidA=$!

minutes=0

while true; do sleep 60; ((minutes++)); echo -e "\033[0;32m$minutes minute(s) elapsed.\033[0m"; done &
pidB=$!

wait $pidA

echo -e "\033[0;32mstack build is finished.\033[0m"

kill -9 $pidB

