.PHONY: all compile dialyzer rel run test

all:
	@$(REBAR3) do clean, compile

compile:
	@$(REBAR3) compile

dialyzer: compile
	@$(REBAR3) dialyzer

test: compile
	@$(REBAR3) ct

run:
	@$(REBAR3) shell

rel:
	@$(REBAR3) release

include rebar3.mk
