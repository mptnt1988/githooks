.PHONY: build clean

build: clean
	@rebar3 compile

clean:
	@rebar3 clean --all

rel:
	@rebar3 rel

clear-rel:
	@rm -rf _build/default/rel/
