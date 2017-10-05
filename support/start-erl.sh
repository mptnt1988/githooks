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

CMD="erl -sname ${NAME} -setcookie ${NAME} -detached ${PATHS} ${OTHERS}"

eval "${CMD}"

TIMEOUT=10
ENDTIME=$((SECONDS+TIMEOUT))
while : ; do
    erl_call -sname ${NAME} -c ${NAME} >/dev/null 2>&1
    [ $? -ne 0 ] && [ $SECONDS -lt $ENDTIME ] || break
done

echo "${CMD}"
