#!/bin/bash

stack --no-terminal install --bench --no-run-benchmarks 
if [ "$TRAVIS_OS_NAME" = "windows" ]; then
  # cp "$(stack path --local-bin)\\fireward.exe" "$(stack path --local-bin)\\${DEPLOY_FILE}" 
  export DEPLOY_FILE=$(stack path --local-bin)\\fireward.exe
else
  export DEPLOY_FILE=$(stack path --local-bin)\\fireward-${TRAVIS_OS_NAME}
  cp "$(stack path --local-bin)\\fireward" "$DEPLOY_FILE"
fi
