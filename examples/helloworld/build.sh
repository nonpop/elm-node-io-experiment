#!/bin/bash -e
./node_modules/.bin/elm make --output build/elm.js src/Main.elm
cp -vu ../../src/actions.js build/ioActions.js
cp -vu src/run.js build/run.js
