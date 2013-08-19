#!/bin/bash

while true
do
  dist/build/hackage-build/hackage-build build
  sleep 900
done
