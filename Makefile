.PHONY: all compile test run rel

all:
	@$(REBAR3) do clean, compile

compile:
	@$(REBAR3) compile

test: compile
	@$(REBAR3) ct

run:
	@$(REBAR3) shell

rel:
	@$(REBAR3) release

include rebar3.mk
