# Usage:
#
# Build the container
# $ docker build . -t siddhu/hackage-server
#
# Run the server
# $ docker run -it -p 8080:8080 siddhu/hackage-server
#

FROM haskell:9.10.1-slim-bullseye

RUN apt-get update && cabal update

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
    libbrotli-dev \
    libgd-dev \
    libicu-dev \
    libssl-dev \
    pkg-config \
    postfix \
    unzip \
    zlib1g-dev

WORKDIR /build
ADD hackage-server.cabal cabal.project ./
RUN cabal build --only-dependencies --enable-tests -j
RUN cabal install hackage-repo-tool
ENV PATH /root/.cabal/bin:$PATH
ADD . ./
RUN hackage-repo-tool create-keys --keys keys
RUN cp keys/timestamp/*.private datafiles/TUF/timestamp.private
RUN cp keys/snapshot/*.private datafiles/TUF/snapshot.private
RUN hackage-repo-tool create-root --keys keys -o datafiles/TUF/root.json
RUN hackage-repo-tool create-mirrors --keys keys -o datafiles/TUF/mirrors.json
RUN cabal build
#RUN cabal test
RUN cabal install all

# setup server runtime environment
WORKDIR /runtime
RUN cp -r /build/datafiles /runtime/datafiles
RUN hackage-server init --static-dir=datafiles
CMD hackage-server run  --static-dir=datafiles --ip=0.0.0.0 --base-uri=http://localhost:8080
EXPOSE 8080
