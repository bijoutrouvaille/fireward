import {spawn, execSync} from 'child_process';

import json = require('../firebase.json');

const EXE = './node_modules/.bin/firebase';
const port = (process.env['FIRESTORE_EMULATOR_HOST']||'').trim().split(':')[1] || json.emulators.firestore.port;

let ready = false;

function start() {

  const exc = (cmd: string)=>{
    const res = execSync(cmd, {
      shell: 'bash',
      uid: process.getuid()
    });
    return res && res.toString().trim() || '';
  }
  

  try {
    const prevPid = exc(`lsof -i :${port} | grep -v COMMAND | awk '{print $2}'`);
    if (prevPid) {
      ready = true;
      return;
    };
  } catch (error: any) {
    console.log('failed with previous pid', {
      // err: error.stderr.toString(),
      // out: error.stdout.toString(),
      msg: error.message,
      error
    });
    process.exit(1);
  }

  const proc = spawn(EXE, `emulators:start --only firestore`.split(' '), {
    stdio: 'pipe'
  });

  proc.stdout.on('data', (data) => {
    console.log(`EMULATOR stdout: ${data}`);
    const msg = `${data}`;
    if (msg.indexOf('It is now safe to connect your app.') > -1) {
      ready = true;
    }
  });


  proc.stderr.on('data', (data) => {
    console.log(`EMULATOR  stderr: ${data}`);
  });

  proc.on('close', (code, signal)=>{
    console.log(`Emulator closed with code ${code}: ${signal}.\nExitting.`)
    process.exit(code || 0);
  })
  proc.on('error', e=>{
    console.error('Emulator error', e);
    process.exit(1);
  })
  proc.on('message', msg => {
    console.log('EMULATOR:', msg);
    if (msg.toString().indexOf('It is now safe to connect your app.') > -1) {
      ready = true;
    }
  })
  proc.on('exit', (code, signal)=>{
    console.log(`Emulator exited with code ${code}: ${signal}.\nExitting.`)
    process.exit(code || 0);
  })
  proc.on('disconnect', ()=>{
    console.log('emulator disconnected');
    process.exit(1);
  })
}

// start();

// export const isEmulatorReady = () => new Promise(res => {
//   const t = setInterval(() => {
//     if (ready) {
//       clearInterval(t);
//       res(null);
//     }
//   }, 100)
// })
