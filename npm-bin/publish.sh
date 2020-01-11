#!/bin/bash

set -ev
./dl-releases.sh
v=$(./fireward-osx -V)
cp ../readme.md ./readme.md
npm version "$v" 
npm publish 
rm ./readme.md
