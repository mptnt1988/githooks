# githooks
Git hooks examples in various languages
## Guides ##
Implement a script at this project root directory, then run below command to create symlink to it as a hook.
  ```bash
  make create REPO=<local_repo_path> TYPE=<client_hook_type> SCRIPT=<script_path>
  ```
*<local_repo_path>* : root directory of a git local repository

*<client_hook_type>* : e.g, pre-push, pre-commit, post-commit,... See support/client_hooks.txt for full list

*<script_path>* : path to the script
## Quick Test (bash) ##
This is to help test the implemented script.

'test' directory and its 2 sub-directories, one is a bare repository 'server' and the other is a cloning repository 'client', are created.

NOTE: 'test' directory is git-ignored by default.

Currently, only Erlang and Python are supported.
### INIT ###
  ```bash
  make init-escript
  make init-py
  ```
### pre-push ###
For pre-push test:
  ```bash
  make push
  ```
### post-push ###
There is no git native support for post-push, and this is just a workaround.

git-push will be wrapped in git-pushq and post-push script is invoked after git-push completes successfully.

Create a symlink first:
  ```bash
  ln -s $PWD/bin/git-pushq.sh <any_dir_included_in_PATH>/git-pushq
  ```
Then it can be used in the same way as git-push:
  ```bash
  git pushq [push_options]
  ```
For post-push test:
  ```bash
  make pushq
  ```
