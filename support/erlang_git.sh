#!/usr/bin/env bash

ROOTPATH=$(dirname $1)/$(basename $1)
CMD=$2  # start | stop | status | pid | attach

export RELX_REPLACE_OS_VARS=true

case ${CMD} in
    start|stop|status|pid|attach)
        ;;
    *)
        echo "Wrong command."
        exit 1
        ;;
esac

GITNAME="$3" ${ROOTPATH}/libs/erlang/git/_build/default/rel/gith/bin/gith ${CMD}
