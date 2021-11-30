FROM erlang:23.1-alpine as backend_build
WORKDIR /app
COPY apps apps
COPY config config
COPY rebar.config .
COPY rebar.lock .
RUN rebar3 as prod tar

FROM node:alpine3.10 as frontend_build
COPY webapp /webapp
WORKDIR /webapp
RUN npm install
RUN npm run build

FROM alpine:3.12
COPY --from=backend_build /app/_build/prod/rel/hohoho/hohoho-0.1.0.tar.gz /opt
RUN set -e \
    && mkdir -p /opt/hohoho \
    && tar -xzf /opt/hohoho-0.1.0.tar.gz -C /opt/hohoho \
    && rm /opt/hohoho-0.1.0.tar.gz
COPY monitor.sh /opt/hohoho
COPY ssh_keys /opt/hohoho/ssh_keys
COPY --from=frontend_build /webapp/build /opt/hohoho/lib/hohoho-0.1.0/priv/webapp/build
WORKDIR /opt/hohoho


RUN set -e \
    && addgroup -g 1000 hohoho \
    && adduser hohoho -u 100 -h /home/hohoho -S -G hohoho \
    && chown -R hohoho:hohoho . \
    && apk add --no-cache ncurses-dev=6.2_p20200523-r0 \
                          openssl-dev=1.1.1i-r0
USER hohoho:hohoho

EXPOSE 8080
EXPOSE 8989

ENTRYPOINT ["./monitor.sh"]

