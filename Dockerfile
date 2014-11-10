from zsol/haskell-platform-2013.2.0.0

env HOME /home/haskell

# dependencies
run sudo apt-get update && sudo apt-get install -yy unzip libicu48 libicu-dev postfix
run cabal update
run curl -LO https://github.com/haskell/hackage-server/archive/master.zip

run unzip master.zip

workdir /home/haskell/hackage-server-master

run cabal install --only-dependencies
run cabal configure && cabal build

cmd echo "Binaries are in ./dist/build/hackage-*"
