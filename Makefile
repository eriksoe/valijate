all: compile

compile:
	./rebar compile skip_deps=true

clean:
	./rebar clean skip_deps=true

test:
	./rebar eunit skip_deps=true


.PHONY: all compile clean test
