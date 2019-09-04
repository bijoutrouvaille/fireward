#!/bin/bash
set -ev

unset CC
if [ "x$GHCVER" = "xhead" ]; then CABALARGS=--allow-newer; fi
export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$HOME/.local/bin:/opt/alex/$ALEXVER/bin:/opt/happy/$HAPPYVER/bin:$HOME/.cabal/bin:$PATH
mkdir -p ~/.local/bin

if [ "$TRAVIS_OS_NAME" = "osx" ]; then
  travis_retry curl --insecure -L https://get.haskellstack.org/stable/osx-x86_64.tar.gz | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
elif [ "$TRAVIS_OS_NAME" = "windows" ]; then
  mkdir -p stackwin
  cd stackwin 
  travis_retry curl -L https://get.haskellstack.org/stable/windows-x86_64.zip > stackwin.zip
  unzip stackwin.zip
  mkdir -p "$APPDATA\\local\\bin"
  export PATH="$APPDATA\\local\\bin":$PATH
  cp stack.exe "$APPDATA\\local\\bin"
  cd ..
else
  travis_retry curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
fi


export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$HOME/.local/bin:/opt/alex/$ALEXVER/bin:/opt/happy/$HAPPYVER/bin:$HOME/.cabal/bin:$PATH
