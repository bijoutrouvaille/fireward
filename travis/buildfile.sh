#!/bin/bash

stack --no-terminal install --bench --no-run-benchmarks 
if [ "$TRAVIS_OS_NAME" = "windows" ]; then
  cp "$(stack path --local-bin)\\fireward.exe" "$(stack path --local-bin)\\${DEPLOY_FILE}" 
else
  cp "$(stack path --local-bin)\\fireward" "$(stack path --local-bin)\\${DEPLOY_FILE}"
fi
