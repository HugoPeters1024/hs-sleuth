FROM haskell:9.2-slim 
WORKDIR /app

RUN apt-get update
RUN apt-get install curl gzip
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /bin
RUN elm --help

COPY . /app/hs-comprehension
# RUN git clone --recursive https://github.com/HugoPeters1024/hs-comprehension
RUN cabal update
# Build takes long so we do it outside the script to get a docker checkpoint
WORKDIR /app/hs-comprehension/server
RUN cabal build -O2
RUN cp /app/hs-comprehension/server/dist-newstyle/build/x86_64-linux/ghc-9.2.4/hs-comprehension-server-0.1.0.0/x/hs-comprehension-server/opt/build/hs-comprehension-server/hs-comprehension-server server

WORKDIR /app/hs-comprehension/frontend
RUN elm make --optimize src/Main.elm
WORKDIR /app/hs-comprehension/server
RUN mkdir static
RUN cp ../frontend/index.html static
RUN cp ../frontend/src/style.css static
RUN cp ../frontend/src/pygments.css static

CMD ["./server", "--direct-root", "../test-project/dumps"]
EXPOSE 8080
