FROM debian:sid

RUN apt-get update && apt-get install --yes lazarus-ide-qt5 unzip

COPY . /example
RUN cd /example && ./lpm update && ./lpm lazarus add 2.0.6 /usr/lib/lazarus/2.0.6 && ./lpm install dcpcrypt-2.0.4.1 && lazbuild example/example.lpi
RUN cd /example && ./lpm build -y example/indyExample.lpi
