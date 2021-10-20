import {readFileSync, writeFileSync} from 'fs';
import {execSync} from 'child_process';

import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'

const isWin = process.platform.toLocaleLowerCase().includes('windows');
const execPath = '../'+execSync(`stack path --dist-dir`, {encoding: 'utf8'}).trim() + `/build/fireward/fireward` 
  + (isWin ? '.exe' : '');
console.log('exec path', execPath)
const tryRead = (path: string) => {
  try {
    return (readFileSync(path, {encoding: 'utf8'}) || '').trim();
  } catch (error) {
    return null;
  }
}
export const getRules = function getRules(wardFile: string) {
  const name = wardFile.replace(/\.ward$/,'');
  const wardName = `./wards/${name}.ward`;
  const tsName = `./wards/${name}.ts`;

  const rules = execSync(execPath + ` -i ${wardName}`, {encoding: 'utf8'});
  const ts = execSync(execPath + ` -i ${wardName} -l typescript`, {encoding: 'utf8'});
  const prevTs = tryRead(tsName);
  if (ts && ts.trim()!==prevTs?.trim()) {
    writeFileSync(tsName, ts, {encoding: 'utf8'});
  }
  return rules
}
// /**
//  * Does 3 things:
//  * - Compiles the ward into rules
//  * - Loads the rules into the firebase app.
//  * - Generates a .ts file inside the wards directory.
//  * @param wardFile ward file name that resides in the wards directory. Extension can be omitted.
//  * @param [app] optional firebase app. By default the first one will be used.
//  */
// export const loadRules = function loadRules(wardFile: string, app: RulesTestContext) {
  
//   const name = wardFile.replace(/\.ward$/,'');
//   const wardName = `./wards/${name}.ward`;
//   const tsName = `./wards/${name}.ts`;

//   const rules = execSync(execPath + ` -i ${wardName}`, {encoding: 'utf8'});
//   const ts = execSync(execPath + ` -i ${wardName} -l typescript`, {encoding: 'utf8'});
//   const prevTs = tryRead(tsName);
//   if (ts && ts.trim()!==prevTs?.trim()) {
//     writeFileSync(tsName, ts, {encoding: 'utf8'});
//   }

//   const projectId = app.options.projectId
//   return firebase.loadFirestoreRules({rules, projectId})
// }