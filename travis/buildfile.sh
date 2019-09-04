#!/bin/bash

stack --no-terminal install --bench --no-run-benchmarks 
if [ "$TRAVIS_OS_NAME" = "windows" ]; then
  tar --force-local -cf $(stack path --local-bin)\\${DEPLOY_FILE}.tar $(stack path --local-bin)\\fireward$OSEXT 
else
  tar -cf $(stack path --local-bin)/${DEPLOY_FILE}.tar $(stack path --local-bin)/fireward$OSEXT 
fi
# zip $(stack path --local-bin)/${DEPLOY_FILE} $(stack path --local-bin)/fireward$OSEXT 
