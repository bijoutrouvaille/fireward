# Integration Tests for Fireward

## Overview

- NodeJS + NPM
- Typescript
- [firestore emulator](https://firebase.google.com/docs/firestore/security/test-rules-emulator).
- Mocha + ts-mocha

## Preparation

Having `NodeJS` and the `stack` tool installed, you'll need to first set up the emulator:

`npm setup`

## Running tests

`npm test`

## Directory Structure

- `test`: actual test files, named <ward-name>.spec.ts
- `wards`: .ward files to test. Emulator runner will generate the .ts files there, which you should commit to git.
- `util`: utility functions.

## Writing Tests

- Copy simple.spec.ts into <my-ward>.spec.ts.
- Create <my-ward>.ward in the `wards` directory.
- Search+replace `simple` in simple.spec.ts with <my-ward>
- write your tests


