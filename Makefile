.PHONY: init push

init-%:
	@support/init.sh $*

push:
	@support/push.sh push

pushq:
	@support/push.sh pushq
