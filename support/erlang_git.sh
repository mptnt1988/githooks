#!/usr/bin/env bash

ROOTPATH=$(dirname $1)/$(basename $1)
CMD=$2

case ${CMD} in
    start|stop|status|pid|attach)
    ;;
    *)
        echo "Wrong command."
        exit 1
        ;;
esac

export RELX_REPLACE_OS_VARS=true

NODE_NAME=githooks_$(ps -p $$ --no-headers -o sid | xargs) ${ROOTPATH}/libs/erlang/git/_build/default/rel/gith/bin/gith ${CMD}
