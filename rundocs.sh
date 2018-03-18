#!/bin/bash

while true
do
  dist/build/hackage-build/hackage-build build --run-time=120 --build-order=recent-uploads-first
  date
  sleep 300
done
