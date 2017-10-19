.PHONY: all build clean release clear-release

all: clean clear-release build release

build:
	@rebar3 compile

clean:
	@rebar3 clean --all

release:
	@rebar3 release

clear-release:
	@rm -rf _build/default/rel/
