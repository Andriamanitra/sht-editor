FROM haskell:9.4-slim
WORKDIR /app
RUN cabal update
COPY ./sht.cabal ./LICENSE /app/
RUN cabal build --only-dependencies -j4
COPY ./app/Main.hs /app/app/Main.hs
RUN cabal build
CMD  ["cabal", "run"]
