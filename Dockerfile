FROM itprotv/stack:tag-2.1.1 AS build

  WORKDIR /root/revolio

  COPY stack.yaml .
  RUN touch revolio.cabal
  RUN stack setup
  RUN stack update

  COPY revolio.cabal .
  RUN stack build --dry-run --prefetch --test
  RUN stack build --only-dependencies --test

  COPY . .
  RUN stack build \
    --copy-bins \
    --no-run-tests \
    --pedantic \
    --test
  RUN stack build --test

FROM debian:stretch-20190610-slim

  RUN apt-get update && apt-get install --assume-yes \
    ca-certificates libgmp-dev netbase
  COPY --from=build /root/.local/bin/revolio /usr/local/bin/revolio
  EXPOSE 80
  CMD revolio \
    --client "$STRATUS_TIME_CLIENT_ID" \
    --host '*' \
    --port 80 \
    --secret "$SLACK_SIGNING_SECRET" \
    --url "$STRATUS_TIME_BASE_URL"
