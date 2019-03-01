FROM haskell:8.6.3 AS build

  WORKDIR /root/revolio

  COPY stack.yaml .
  RUN stack setup
  RUN stack update

  COPY revolio.cabal .
  RUN stack build --dry-run --prefetch --test
  RUN stack build --only-dependencies --test

  COPY . .
  RUN stack build \
    --copy-bins \
    --local-bin-path /usr/local/bin \
    --no-run-tests \
    --pedantic \
    --test
  RUN stack build --test

FROM debian:9.7-slim

  RUN apt-get update && apt-get install --assume-yes \
    ca-certificates libgmp-dev netbase
  COPY --from=build /usr/local/bin/revolio /usr/local/bin/revolio
  EXPOSE 80
  CMD revolio \
    --client "$STRATUS_TIME_CLIENT_ID" \
    --host '*' \
    --port 80 \
    --secret "$SLACK_SIGNING_SECRET"
