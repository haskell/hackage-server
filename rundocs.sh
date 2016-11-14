#!/bin/bash

while true
do
  dist/build/hackage-build/hackage-build build --run-time=120
  sleep 300
done
