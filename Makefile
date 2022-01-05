
# Tips:
# To run tests for particular LTS with (e.g.):
#    make STACK_LTS=lts-14 test

.PHONY: test-lts-3

.DELETE_ON_ERROR:

SHELL=bash

# default stack LTS to use
STACK_LTS=lts-11

ifeq ($(STACK_LTS), lts-3)
STACK=stack-1.9.3
else
STACK=stack
endif

#STACK_ARGS = --no-run-tests

# Run tests for a particular LTS with (e.g.):
#    make STACK_LTS=lts-14 test
test:
	$(STACK) --stack-yaml=stack-$(STACK_LTS).yaml test $(STACK_ARGS) --fast \
	  --flag hup:EnableWebTests --flag hup:BuildStackBasedTests

# Test oldest known working stack LTS
test-lts-3:
	make STACK_LTS=lts-3 test

HUP_STACK_YAML=stack-lts-11.yaml

hup:
	stack --stack-yaml=$(HUP_STACK_YAML) --local-bin-path . build --copy-bins

README.md:

%.md: %.pmd ./hup
	pweave --format=markdown $<

##
# Some platform-specific commands/invocations

PLATFORM=Linux
# could also be "macOS" or "Windows"

GUNZIP_Linux=gunzip -f -k
GUNZIP_macOS=gunzip -f -k
GUNZIP_Windows=7z -y x

GUNZIP=$(GUNZIP_$(PLATFORM))

GOSS_Linux_EXE=goss-linux-amd64
GOSS_macOS_EXE=goss-alpha-darwin-amd64
GOSS_Windows_EXE=goss-alpha-windows-amd64.exe

GOSS_EXE=$(GOSS_$(PLATFORM)_EXE)

HACKAGE_SERVER_IMG=phlummox/hackage-server:0.1.0

.version:
	./hup-version.pl $(HUP_STACK_YAML) > .version
	grep '^[0-9]' .version >/dev/null

# cache version in .version as a side-effect
ifndef HUP_VERSION
HUP_VERSION :=$(or $(shell cat .version),$(shell $(MAKE) HUP_VERSION=xxx .version >/dev/null && cat .version))
endif

GOSS_VERSION=v0.3.16
GOSS_BASE_URL=https://github.com/aelsabbahy/goss/releases/download/$(GOSS_VERSION)
GOSS_URL=$(GOSS_BASE_URL)/$(GOSS_EXE)

$(GOSS_EXE):
	set -x; set -v; \
	goss_sha256=`curl -w '%{filename_effective}' --remote-header-name -s -L -O $(GOSS_URL).sha256` && \
	goss_exe=`curl -w '%{filename_effective}' --remote-header-name -s -L -O $(GOSS_URL)` && \
	sha256sum -c $$goss_sha256 && \
	chmod a+rx $$goss_exe
	[ -x $(GOSS_EXE) ]

HUP_TGZ=hup-$(HUP_VERSION).tar.gz
HUP_TGZ_URL=http://localhost:8080/package/hup-$(HUP_VERSION)/hup-$(HUP_VERSION).tar.gz
HUP_DOCS_TGZ=hup-$(HUP_VERSION)-docs.tar.gz
HUP_DOCS_TGZ_URL=http://localhost:8080/package/hup-$(HUP_VERSION)/docs.tar

# wait for a URL to become available
GOSS_WAITFOR_URL=GOSS_USE_ALPHA=1 ./$(GOSS_EXE) validate --retry-timeout 90s --sleep 4s

# qcow images used when testing with Qemu
HACKAGE_QCOW_FILE=hackage_server_0.0.1.qcow2
HACKAGE_QCOW_URL=https://github.com/phlummox/hup/releases/download/v0.3.0.2/qemu-image

$(HACKAGE_QCOW_FILE):
	curl -L -o $(HACKAGE_QCOW_FILE).gz $(HACKAGE_QCOW_URL)
	$(GUNZIP) $(HACKAGE_QCOW_FILE).gz

# work around https://github.com/phlummox/hup/issues/12;
# can't pass --stack-yaml to hup, so we need to have
# a concrete stack.yaml file
stack.yaml: $(HUP_STACK_YAML)
	cp $(HUP_STACK_YAML) stack.yaml

$(HUP_TGZ): hup stack.yaml
	./hup packbuild

$(HUP_DOCS_TGZ): hup stack.yaml
	./hup docbuild

serve:
	docker -D run --rm -it -p 8080:8080 --hostname localhost $(HACKAGE_SERVER_IMG) \
		bash -c 'set -x; rm -f state/db/*/*/*.lock && rm -f state/db/*/*.lock && hackage-server run -v --ip=0.0.0.0 --static-dir=datafiles'

# Basic smoke test + integration test against hackage-server.
#
# Runs a docker container to test basic hup functionality
# against an actual instance of hackage-server.
#
# The container won't be removed properly if the test fails;
# but we expect this test to largely be run in CI environments
# where this isn't an issue.
docker-core-test: hup $(HUP_TGZ) $(HUP_DOCS_TGZ) $(GOSS_EXE)
	set -vx; \
	set -euo pipefail; \
	ctr_id=`./start-hackage-server.sh $(PLATFORM)` || true && \
	echo $$ctr_id && \
	$(GOSS_WAITFOR_URL) && \
	echo hup tgz: $(HUP_TGZ) && \
	echo hup tgz: $(HUP_DOCS_TGZ) && \
	`# build tgz files` \
	./hup packup  --server=http://localhost:8080/ -u admin -p admin $(HUP_TGZ) && \
	./hup docup   --server=http://localhost:8080/ -u admin -p admin $(HUP_DOCS_TGZ) && \
	`# download and test src tgz file` \
	curl -L -o hup-$(HUP_VERSION).tar.gz.got $(HUP_TGZ_URL)  && \
	read -d ' ' expected_sha256sum  <<<$$(sha256sum $(HUP_TGZ)    ) && \
	read -d ' ' actual_sha256sum    <<<$$(sha256sum $(HUP_TGZ).got) && \
	[ "$${actual_sha256sum}" = "$${expected_sha256sum}" ] && \
	`# download and test doc tgz file` \
	`# (NB that downloaded file is a .tar, not a .tar.gz)` \
	curl -L -o hup-$(HUP_VERSION)-docs.tar.got $(HUP_DOCS_TGZ_URL) && \
	$(GUNZIP) $(HUP_DOCS_TGZ) && \
	read -d ' ' expected_sha256sum  <<<$$(sha256sum hup-$(HUP_VERSION)-docs.tar    ) && \
	read -d ' ' actual_sha256sum    <<<$$(sha256sum hup-$(HUP_VERSION)-docs.tar.got) && \
	[ "$${actual_sha256sum}" = "$${expected_sha256sum}" ] && \
	./stop-hackage-server.sh $(PLATFORM) $$ctr_id

docker-test:
	docker pull $(HACKAGE_SERVER_IMG)
	-docker stop -t 1 hackage-server-ctr
	$(MAKE) docker-core-test

clean:
		-rm -f dgoss goss goss-* hup $(HUP_TGZ)* $(HUP_DOCS_TGZ)* hup-$(HUP_VERSION)-docs.tar* .version

extra-clean: clean
	-stack --stack-yaml=$(HUP_STACK_YAML) clean
	-rm -f README.md

