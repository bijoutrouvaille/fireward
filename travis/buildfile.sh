#!/bin/bash

stack --no-terminal install --bench --no-run-benchmarks 
# tar -cf $(stack path --local-bin)/${DEPLOY_FILE}.tar $(stack path --local-bin)/fireward$OSEXT 
zip $(stack path --local-bin)/${DEPLOY_FILE} $(stack path --local-bin)/fireward$OSEXT 
