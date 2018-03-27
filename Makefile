PROJECT = feature_flag_proxy
PROJECT_DESCRIPTION = Feature Flag Proxy
PROJECT_VERSION = 0.1.0


DEPS = cowboy gun
dep_cowboy_commit = 2.2.2

DEP_PLUGINS = cowboy

include erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
