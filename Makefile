.PHONY: build init push

build:
	@make build -C libs/erlang/git/

init-%:
	@support/init.sh $*

push:
	@support/push.sh push

pushq:
	@support/push.sh pushq
