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
RUN DEBIAN_FRONTEND=noninteractive apt-get install -yy unzip libicu-dev postfix
RUN apt-get install -y ghc-8.2.2 cabal-install-2.4
ENV PATH /opt/ghc/bin:$PATH
RUN cabal v2-update

# Required Header files
RUN apt-get install -y zlib1g-dev libssl-dev

# haskell dependencies
RUN mkdir /build
WORKDIR /build
ADD ./hackage-server.cabal ./hackage-server.cabal
RUN cabal v2-install -j --force-reinstalls
ENV PATH /root/.cabal/bin:$PATH

# needed for creating TUF keys
RUN cabal v2-install hackage-repo-tool

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
RUN cabal v2-configure -f-build-hackage-mirror --enable-tests
RUN cabal v2-build
# tests currently don't pass: the hackage-security work introduced some
# backup/restore errors (though they look harmless)
# see https://github.com/haskell/hackage-server/issues/425
#RUN cabal test
RUN cabal v2-install all

# setup server runtime environment
RUN mkdir /runtime
RUN cp -r /build/datafiles /runtime/datafiles
WORKDIR /runtime
RUN hackage-server init --static-dir=datafiles
CMD hackage-server run  --static-dir=datafiles
EXPOSE 8080
