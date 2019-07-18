FROM haskell:8.6

RUN mkdir -p /opt/src/fireward

RUN apt-get update
RUN apt-get install -q --assume-yes git
RUN git clone https://github.com/bijoutrouvaille/fireward.git /opt/src/fireward

WORKDIR /opt/src/fireward

RUN stack install

ENTRYPOINT ["/root/.local/bin/fireward"]
CMD ["--lang=rules"]
