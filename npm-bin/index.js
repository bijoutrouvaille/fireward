#!/usr/bin/env node

var spawn = require('child_process').spawn;

var path = require('path');

const bin =
  process.platform === 'darwin'
    ? path.join(__dirname, 'fireward-osx') :
  process.platform === 'linux' && process.arch === 'x64'
    ? path.join(__dirname, 'fireward-linux') :
  process.platform === 'win32' &&  process.arch === 'x64'
    ? path.join(__dirname, 'fireward.exe') :
  null;


var input = process.argv.slice(2);

if (bin !== null) {
  spawn(bin, input, {stdio: 'inherit'})
    .on('exit', process.exit);
} else {
  throw new Error('Platform not supported.');
}
