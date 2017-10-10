.PHONY: build init push

defined_check = \
    $(strip $(foreach 1,$1,$(call __defined_check,$1,$(strip $(value 2)))))
__defined_check = \
    $(if $(value $1),,$(error Undefined $1$(if $2, ($2))))

build:
	@make build -C libs/erlang/git/

# For using
create:
	$(call defined_check, REPO, which repository to add the hook)
	$(call defined_check, TYPE, type of client hook)
	$(call defined_check, SCRIPT, script name)
	@support/create.sh $(REPO) $(TYPE) $(SCRIPT)

# For testing
init-%:
	@support/init.sh $*

push:
	@support/push.sh push

pushq:
	@support/push.sh pushq
