.PHONY: all schema

SCHEMA_DIR ?= /tmp/volunteer_mgr

all: clean compile

schema: compile $(SCHEMA_DIR)/schema.DAT

$(SCHEMA_DIR)/schema.DAT:
	escript $(CURDIR)/tools/create_schema $(SCHEMA_DIR)

include rebar3.mk
