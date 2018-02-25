FROM debian:9.3-slim AS build

  # Install dependencies.
  RUN apt-get update && apt-get install --assume-yes gcc libgmp-dev make netbase wget xz-utils zlib1g-dev

  # Install Stack.
  WORKDIR /root/stack
  RUN wget https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64.tar.gz
  RUN tar --extract --file stack-1.6.5-linux-x86_64.tar.gz
  RUN cp stack-1.6.5-linux-x86_64/stack /usr/local/bin

  # Install GHC.
  WORKDIR /root/reveille
  COPY stack.yaml .
  RUN stack setup

  # Build dependencies.
  COPY package.yaml .
  RUN stack build --only-dependencies

  # Build Reveille.
  COPY . .
  RUN stack build --copy-bins

FROM debian:9.3-slim

  RUN apt-get update && apt-get install --assume-yes ca-certificates libgmp-dev netbase
  COPY --from=build /root/.local/bin/reveille /usr/local/bin
  EXPOSE 8080
  CMD reveille
