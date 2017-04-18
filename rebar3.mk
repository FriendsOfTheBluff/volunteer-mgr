REBAR3_URL := https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 := $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell test -x `which rebar3` 2>/dev/null && which rebar3 || echo "$(CURDIR)/rebar3")

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 := $(CURDIR)/rebar3
endif

rebar3: $(REBAR3)

$(REBAR3):
	curl -Lo $(REBAR3) $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x $(REBAR3)
