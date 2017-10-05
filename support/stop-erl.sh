#!/usr/bin/env bash

NAME=${1}

if [ -z "${NAME}" ]; then
    echo "No name specified."
    exit 1
fi

CMD="erl_call -sname ${NAME} -c ${NAME} -q"

eval "${CMD}"

TIMEOUT=10
ENDTIME=$((SECONDS+TIMEOUT))
while : ; do
    erl_call -sname ${NAME} -c ${NAME} >/dev/null 2>&1
    [ $? -eq 0 ] && [ $SECONDS -lt $ENDTIME ] || break
done

echo "${CMD}"
