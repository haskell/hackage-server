from zsol/haskell-platform-2013.2.0.0

env HOME /home/haskell

# dependencies
run sudo apt-get install unzip libicu48 libicu-dev
run cabal update
run curl -LO https://github.com/haskell/hackage-server/archive/master.zip

run unzip master.zip

workdir /home/haskell/hackage-server-master

run cabal install --only-dependencies
run cabal configure && cabal build

run ./dist/build/hackage-server/hackage-server init --static-dir=datafiles/

entrypoint ["./dist/build/hackage-server/hackage-server", "run", "--static-dir=datafiles/"]
expose 8080
