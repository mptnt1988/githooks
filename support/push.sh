#!/usr/bin/env bash

cd test/client/ &&
    echo text>>file &&
    git add file &&
    git commit -m "Comment" &&
    git "${1}"
