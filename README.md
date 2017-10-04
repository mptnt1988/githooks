# githooks
Git hooks examples in various languages

## Quick Test (bash) ##
### INIT ###
'test' directory and its 2 sub-directories, one is a bare repository 'server' and the other is a cloning repository 'client', are created.

NOTE: 'test' directory is ignored by default.
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
There is no git support for post-push, and this is just a workaround.

git-push will be wrapped and post-push script is invoked after git-push completes successfully.

Create a symlink first:
  ```bash
  ln -s $PWD/git-pushq.sh <any_dir_included_in_PATH>/git-pushq
  ```
Then it can be used in the same way as git-push:
  ```bash
  git pushq [push_options]
  ```
For post-push test:
  ```bash
  make pushq
  ```