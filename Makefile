.PHONY: all rel

all:
	@$(REBAR3) do clean, compile

run:
	@$(REBAR3) shell

rel:
	@$(REBAR3) release

include rebar3.mk
