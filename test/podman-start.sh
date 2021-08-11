#!/bin/sh

set -e

podman pod stop didit-pod || true
podman pod rm didit-pod || true

BASE_HASH=$(sha512sum src/didit.asd | cut -b 1-8)1
podman images | grep ${BASE_HASH} > /dev/null 2>&1 || podman build -f build/Dockerfile.base --no-cache -t didit-base:${BASE_HASH}1 .

podman build -f build/Dockerfile -t didit:latest --no-cache --from didit-base:${BASE_HASH}1 src

#cat test/test-pod1.yml | sed -e 's/\/home\/runner\/work\/didit\/didit\/test\/config.ini/\/home\/green\/git\/didit\/test\/private1.ini/' | podman play kube -
#cat test/test-pod2.yml | sed -e 's/\/home\/runner\/work\/didit\/didit\/test\/config.ini/\/home\/green\/git\/didit\/test\/private2.ini/' | podman play kube -
#cat test/test-pod3.yml | sed -e 's/\/home\/runner\/work\/didit\/didit\/test\/config.ini/\/home\/green\/git\/didit\/test\/private3.ini/' | podman play kube -

podman play kube test/test3-pod.yml

timeout -s TERM 20 bash -c 'while [[ "$(curl -s -o /dev/null -L -w ''%{http_code}'' http://localhost:8080/health)" != "200" ]]; do echo "Waiting for http://localhost:8080/health" && sleep 2; done'
#timeout -s TERM 20 bash -c 'while [[ "$(curl -s -o /dev/null -L -w ''%{http_code}'' http://localhost:8180/health)" != "200" ]]; do echo "Waiting for http://localhost:8080/health" && sleep 2; done'
#timeout -s TERM 20 bash -c 'while [[ "$(curl -s -o /dev/null -L -w ''%{http_code}'' http://localhost:8280/health)" != "200" ]]; do echo "Waiting for http://localhost:8080/health" && sleep 2; done'

echo ===========================================================================
echo
echo Application: http://localhost:8080
echo
echo Prometheus : http://localhost:9101/metrics
echo
echo Use emacs to \'sly-connect\' to localhost port 4005
echo
echo Run \'make podman-stop\' to stop the test pod
echo
echo ===========================================================================

podman logs -f didit-pod-didit1
