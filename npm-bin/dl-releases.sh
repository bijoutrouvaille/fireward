#!/bin/bash
set -ev

# v=$(cd .. && stack exec fireward -- -V)
v=$(git describe --tags `git rev-list --tags --max-count=1`)
export files=(fireward-linux fireward-osx fireward.exe)
for f in ${files[*]}; do
  if [ -f "$f" ]; then 
    rm $f
  fi
done
for f in ${files[*]}; do
  echo "downloading $f"
  curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$v/$f -o $f
  chmod 755 $f
done

# curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$v/fireward-linux -o fireward-linux
# curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$v/fireward-osx -o fireward-osx
# curl -L https://github.com/bijoutrouvaille/fireward/releases/download/$v/fireward.exe -o fireward.exe

