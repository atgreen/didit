all:
	@echo "Supported targets:"
	@echo " clean		- clean the source tree"
	@echo " podman-start	- run in podman using test/test-pod.yml"
	@echo " podman-stop	- stop the test pod"
	@echo " run		- run locally"

run:
	sbcl --eval '(pushnew (truename "./src") ql:*local-project-directories* )' \
	     --eval '(ql:register-local-projects)' \
	     --eval '(ql:quickload :didit)' \
	     --eval '(didit:start-server)'

podman-start:
	sh test/podman-start.sh

podman-stop:
	-podman pod stop didit-pod
	-podman pod rm didit-pod

clean:
	@rm -rf system-index.txt *~
