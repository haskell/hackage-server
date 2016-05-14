FROM haskell:7.10.2

# dependencies
RUN apt-get update && apt-get install -yy unzip libicu-dev postfix
RUN cabal update

# haskell dependencies
RUN mkdir /build
WORKDIR /build
ADD ./hackage-server.cabal ./hackage-server.cabal
RUN cabal sandbox init
RUN cabal install --only-dependencies --enable-tests -j
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
CMD hackage-server run --static-dir=datafiles
EXPOSE 8080
