FROM haskell:9.2.8-slim

COPY . "/app"
WORKDIR "/app"
RUN stack --local-bin-path build --system-ghc install

ENTRYPOINT ["/app/build/interpreter"]
