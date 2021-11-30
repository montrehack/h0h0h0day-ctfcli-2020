FROM ubuntu:latest
RUN apt-get -y update && apt-get install -y xinetd && \
    useradd -m elfo && \
    chmod 700 -R /home/elfo

COPY bin/santa-factory /home/elfo/
COPY bin/xinetd.conf /etc/xinetd.d/santa-factory
COPY bin/flag.txt /home/elfo/flag.txt

CMD ["/usr/sbin/xinetd", "-dontfork"]