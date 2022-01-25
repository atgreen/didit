FROM quay.io/atgreen/didit-base:latest

COPY . /tmp/src
ARG APP_VERSION=APP_VERSION
ENV APP_VERSION=${APP_VERSION}
RUN APP_SYSTEM_NAME=didit /usr/libexec/s2i/assemble
WORKDIR /opt/app-root
CMD DEV_BACKEND=slynk APP_SYSTEM_NAME=didit APP_EVAL="\"(didit:start-server)\"" PATH=~/.local/bin:$PATH /usr/libexec/s2i/run
