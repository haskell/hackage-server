#!/bin/bash

cabal update hackage.haskell.org,2022-08-27T00:00:00Z
cabal build all --enable-tests
