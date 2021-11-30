FROM alpine:latest

RUN apk add --no-cache openssh python3 py3-pip gcc python3-dev musl-dev \
    && pip install getch \
    && rm -rf /tmp/* /var/cache/apk/*

ADD ./src /home/h3s

RUN adduser -s /home/h3s/shell.py -k /dev/null -H -D h3s \
&& echo "h3s:santa" | chpasswd \
&& echo -e "Port 22\nProtocol 2\nMaxSessions 100\nPasswordAuthentication yes\nPubkeyAuthentication no\nPermitRootLogin no\nClientAliveInterval 180\nAllowAgentForwarding no\nAllowTcpForwarding no\nAllowStreamLocalForwarding no\nGatewayPorts no\nX11Forwarding no\nPrintMotd no\nAllowUsers h3s\nSubsystem sftp /bin/false\nPermitTunnel no" > /etc/ssh/sshd_config \
&& chmod 0444 /home/h3s/files/0000 /home/h3s/files/0F20 /home/h3s/files/1337 /home/h3s/files/7331 \
&& chown -R h3s:h3s /home/h3s/files

EXPOSE 22
ENTRYPOINT ["/home/h3s/entrypoint.sh"]
CMD ["/usr/sbin/sshd", "-D"]
