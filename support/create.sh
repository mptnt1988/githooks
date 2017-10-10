#!/usr/bin/env bash

REPO=$1
TYPE=$2
SCRIPT=$3

if [ ! -d "${REPO}/.git" ]; then
    echo "${REPO} is not a git repository."
    exit 1
fi

if [[ ! "${TYPE}" =~ $(echo ^\($(paste -sd'|' support/client_hooks.txt)\)$) ]]; then
    echo "${TYPE} is not a git client hook."
    exit 1
fi

if [ ! -x "${SCRIPT}" ]; then
    echo "${SCRIPT} does not exist or is not executable."
    exit 1
fi

SYMLINK=${REPO}/.git/hooks/${TYPE}

if [ -e "${SYMLINK}" ]; then
    echo -n "Hook exists. Do you want to overwrite it (y/N)? "
    read overwrite
    case $overwrite in
        y|Y)
            rm -f ${SYMLINK}
            ;;
        *)
            echo "Nothing to do."
            exit 0
            ;;
    esac
fi

ln -s $PWD/${SCRIPT} ${SYMLINK}
