FROM debian:bullseye-slim

RUN mkdir -p /opt/data/in && mkdir -p /opt/data/out && mkdir -p /opt/bin
COPY .bin/panrec /opt/bin/panrec

WORKDIR /opt/bin

ENTRYPOINT ["./panrec"]
