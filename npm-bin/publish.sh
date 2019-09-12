#!/bin/bash

set -ev
v=$(cd .. && stack exec fireward -- -V)
echo $v
./dl-releases.sh
npm version "$v" 
npm publish 
