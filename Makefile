.PHONY: all schema

PROJDIR := $(realpath $(CURDIR))

SCHEMA_DIR ?= /tmp/volunteer_mgr
NODENAME ?= volunteer_mgr@localhost.localdomain
EBIN ?= $(PROJDIR)/_build/default/lib/volunteer_mgr/ebin

all: clean compile

schema: compile $(SCHEMA_DIR)/schema.DAT

$(SCHEMA_DIR)/schema.DAT:
	mkdir -p "$(SCHEMA_DIR)"
	erl +B -noshell -noinput -name "$(NODENAME)" -pa "$(EBIN)" \
		-mnesia debug verbose \
		-mnesia dir '"$(SCHEMA_DIR)"' \
		-mnesia schema_location disc \
		-eval 'volmgr_db_schema:init_schema()' \
		-eval 'init:stop(0)'

include rebar3.mk
