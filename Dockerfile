FROM containerlisp/lisp-10-ubi8

COPY . /tmp/src
ARG DIDIT_VERSION=DIDIT_VERSION
ENV DIDIT_VERSION=${DIDIT_VERSION}
RUN APP_SYSTEM_NAME=didit /usr/libexec/s2i/assemble
USER 0
RUN mkdir -p /var/didit/config && chown -R 1001:0 /var/didit
USER 1001
CMD DEV_BACKEND=slynk APP_SYSTEM_NAME=didit APP_EVAL="\"(didit:start-didit t)\"" /usr/libexec/s2i/run
