# githooks
Git hooks examples in various languages

## Quick Test (bash) ##
NOTE: 'test' directory is ignored by default.
### pre-push ###
Init:
  ```bash
  mkdir test && cd test
  git init --bare server
  git init client
  cd client
  git remote add origin $PWD/../server/
  git commit --allow-empty -m "Init commit"
  git push --set-upstream origin master
  ln -s $PWD/../../pre-push.py .git/hooks/pre-push
  ```
Test:
  ```bash
  echo text>>file && git add file && git commit -m "Comment" && git push
  ```
