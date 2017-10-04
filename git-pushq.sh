#!/usr/bin/env bash

POST_PUSH="$(git rev-parse --git-dir)/hooks/post-push"
git push "$@"

if [ $? -eq 0 ]; then
    if [ -x "$POST_PUSH" ]; then
        exec $POST_PUSH
    else
        echo "No executable post-push script."
    fi
else
    echo "post-push does not run because of failed push."
    exit 1
fi
