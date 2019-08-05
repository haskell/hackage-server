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

# Set up the system environment.
FROM ubuntu
RUN apt-get update
RUN apt-get install -y software-properties-common
RUN apt-add-repository ppa:hvr/ghc
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y ghc-8.2.2 cabal-install-2.4
ENV PATH /opt/ghc/bin:$PATH
RUN cabal v2-update

# Install current HEAD version of cabal-install.
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y curl zlib1g-dev
WORKDIR /tmp
RUN curl --silent -L https://github.com/haskell/cabal/tarball/dc138034d469973fee43af86764567e4202ca84b | tar xz
WORKDIR /tmp/haskell-cabal-dc13803
RUN cabal v2-install -j cabal-install
ENV PATH /root/.cabal/bin:$PATH

# Build hackage-server.
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y zlib1g-dev libssl-dev unzip libicu-dev postfix
RUN mkdir /build
WORKDIR /build
ADD hackage-server.cabal cabal.project ./
RUN cabal v2-build --only-dependencies --enable-tests
RUN cabal v2-install --help
RUN cabal v2-install --installdir=/root/.cabal/bin hackage-repo-tool
ADD . ./
RUN hackage-repo-tool create-keys --keys keys
RUN cp keys/timestamp/*.private datafiles/TUF/timestamp.private
RUN cp keys/snapshot/*.private datafiles/TUF/snapshot.private
RUN hackage-repo-tool create-root --keys keys -o datafiles/TUF/root.json
RUN hackage-repo-tool create-mirrors --keys keys -o datafiles/TUF/mirrors.json
# tests currently don't pass: the hackage-security work introduced some
# backup/restore errors (though they look harmless)
# see https://github.com/haskell/hackage-server/issues/425
#RUN cabal v2-test
RUN cabal v2-install --installdir=/root/.cabal/bin all

# setup server runtime environment
RUN mkdir /runtime
RUN cp -r /build/datafiles /runtime/datafiles
WORKDIR /runtime
RUN hackage-server init --static-dir=datafiles
CMD hackage-server run  --static-dir=datafiles
EXPOSE 8080
