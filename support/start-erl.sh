#!/usr/bin/env bash

NAME=${1}
PATHS=${2}
OTHERS=${3}

if [ -z "${NAME}" ]; then
    echo "No name specified."
    exit 1
fi

if [ -z "${PATHS}" ]; then
    PATHS=""
else
    PATHS="-pa ${PATHS}"
fi

CMD="erl -sname ${NAME} -detached ${PATHS} ${OTHERS}"

eval "${CMD}"

echo "${CMD}"
