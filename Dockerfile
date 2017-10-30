# Usage:
#
# Build the container
# $ docker build . -t siddhu/hackage-server
#
# Shell into the container
# $ docker run -it -p 8080:8080 siddhu/hackage-server /bin/bash
#
# Run the server
# Docker> # hackage-server run --static-dir=datafiles
#

FROM ubuntu

RUN apt-get update

# Install apt-add-repository
RUN apt-get install -y software-properties-common

# Use Herbert's PPA on Ubuntu for getting GHC and cabal-install
RUN apt-add-repository ppa:hvr/ghc

RUN apt-get update

# Dependencies
RUN apt-get install -yy unzip libicu-dev postfix
RUN apt-get install -y ghc-8.2.1 cabal-install-2.0
ENV PATH /opt/ghc/bin:$PATH
RUN cabal update

# Required Header files
RUN apt-get install -y zlib1g-dev libssl-dev

# haskell dependencies
RUN mkdir /build
WORKDIR /build
ADD ./hackage-server.cabal ./hackage-server.cabal
RUN cabal sandbox init
# TODO: Switch to Nix-style cabal new-install
RUN cabal install --only-dependencies --enable-tests -j --force-reinstalls
ENV PATH /build/.cabal-sandbox/bin:$PATH

# needed for creating TUF keys
RUN cabal install hackage-repo-tool

# add code
# note: this must come after installing the dependencies, such that
# we don't need to rebuilt the dependencies every time the code changes
ADD . /build

# generate keys (needed for tests)
RUN hackage-repo-tool create-keys --keys keys
RUN cp keys/timestamp/*.private datafiles/TUF/timestamp.private
RUN cp keys/snapshot/*.private datafiles/TUF/snapshot.private
RUN hackage-repo-tool create-root --keys keys -o datafiles/TUF/root.json
RUN hackage-repo-tool create-mirrors --keys keys -o datafiles/TUF/mirrors.json

# build & test & install hackage
RUN cabal configure -f-build-hackage-mirror --enable-tests
RUN cabal build
# tests currently don't pass: the hackage-security work introduced some
# backup/restore errors (though they look harmless)
# see https://github.com/haskell/hackage-server/issues/425
#RUN cabal test
RUN cabal copy && cabal register

# setup server runtime environment
RUN mkdir /runtime
RUN cp -r /build/datafiles /runtime/datafiles
WORKDIR /runtime
RUN hackage-server init --static-dir=datafiles
CMD hackage-server run  --static-dir=datafiles
EXPOSE 8080
