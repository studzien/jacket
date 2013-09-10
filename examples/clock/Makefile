all: compile

compile: deps
	./rebar compile

deps: rebar
	./rebar get-deps

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
