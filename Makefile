PROJECT = feature_flag_proxy
PROJECT_DESCRIPTION = Feature Flag Proxy
PROJECT_VERSION = 0.1.0


DEPS = cowboy gun jiffy
dep_cowboy_commit = 2.4.0

DEP_PLUGINS = cowboy

include erlang.mk

ifeq ($(MODE),development)
$(info MODE is development, will compile despite warnings.)
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
else
$(info Treating warnings as error. Set MODE=development to override.)
endif

build-container:
	docker build  -t rciorba/feature_flag_proxy -f Dockerfile .
