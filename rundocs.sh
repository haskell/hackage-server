#!/bin/bash

while true
do
  dist/build/hackage-build/hackage-build build --run-time=25
  sleep 300
done
