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

FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y software-properties-common
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y unzip libicu-dev postfix zlib1g-dev libssl-dev

RUN apt-add-repository ppa:hvr/ghc
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y ghc-8.2.2 cabal-install-3.0
ENV PATH /opt/ghc/bin:$PATH
RUN cabal v2-update
RUN mkdir /build
WORKDIR /build
ADD hackage-server.cabal cabal.project ./
RUN cabal v2-build --only-dependencies --enable-tests -j
RUN cabal v2-install hackage-repo-tool
ENV PATH /root/.cabal/bin:$PATH
ADD . ./
RUN hackage-repo-tool create-keys --keys keys
RUN cp keys/timestamp/*.private datafiles/TUF/timestamp.private
RUN cp keys/snapshot/*.private datafiles/TUF/snapshot.private
RUN hackage-repo-tool create-root --keys keys -o datafiles/TUF/root.json
RUN hackage-repo-tool create-mirrors --keys keys -o datafiles/TUF/mirrors.json
RUN cabal v2-build
# tests currently don't pass: the hackage-security work introduced some
# backup/restore errors (though they look harmless)
# see https://github.com/haskell/hackage-server/issues/425
#RUN cabal v2-test
RUN cabal v2-install all

# setup server runtime environment
RUN mkdir /runtime
RUN cp -r /build/datafiles /runtime/datafiles
WORKDIR /runtime
RUN hackage-server init --static-dir=datafiles
CMD hackage-server run  --static-dir=datafiles
EXPOSE 8080
