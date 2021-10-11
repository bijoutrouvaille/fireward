#!/bin/bash

set -ev
branch=$(git branch --show-current)
if [ "$1" != "beta" ] && [ $branch != "master" ]; then
  echo non-beta releases must be published from the master branch.
  exit 1
fi
./dl-releases.sh
# v=$(./fireward-osx -V)
v=$(git describe --tags `git rev-list --tags --max-count=1`)
cp ../readme.md ./readme.md
npm version "$v" 
if [ "$1" = "beta" ]; then
  npm publish --tag beta
else 
  npm publish 
fi
npm publish 
rm ./readme.md
