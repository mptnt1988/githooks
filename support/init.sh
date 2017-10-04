#!/usr/bin/env bash

create_hooks () {
    ARGS=("$@")
    for script in "${@:2}"
    do
        script_path=$PWD/../../"${script}.${1}"
        if [ -x "${script_path}" ]; then
            ln -s ${script_path} .git/hooks/"${script}"
        else
            echo "$(basename ${script}) does not exist or is not executable."
            cd $CWD && rm -rf test
            exit 1
        fi
    done
}

CWD=$PWD
rm -rf test
mkdir test &&
    cd test
git init --bare server
git init client
cd client
git remote add origin $PWD/../server/
git commit --allow-empty -m "Init commit"
git push --set-upstream origin master
create_hooks "${1}" pre-push post-push
