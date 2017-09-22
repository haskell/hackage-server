#!/bin/bash

while true
do
  dist/build/hackage-build/hackage-build build --run-time=120 --build-order=recent-uploads-first
  sleep 300
done
