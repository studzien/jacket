.PHONY: all clean deps distclean doc test compile

REBAR ?= ./rebar

all: compile

test: compile
	@$(REBAR) skip_deps=true eunit

deps: rebar
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

doc:
	@$(REBAR) doc

compile: deps
	@$(REBAR) compile

rebar:
	curl http://cloud.github.com/downloads/basho/rebar/rebar > rebar
	chmod +x $@
