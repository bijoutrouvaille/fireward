
import {expect} from 'chai';
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {OptionalTypesExample as Test} from '../wards/optionalTypes';
import {isEmulatorReady} from './../emulator'

const WARD_NAME = 'optionalTypes';
type App = ReturnType<typeof firebase.initializeTestApp>;
let app: App;

before(async function(){
  this.timeout('10s')
  await isEmulatorReady();
})

describe(WARD_NAME, function(){
  let count = 0;
  let projectId = ''
  beforeEach(async function(){
    count++;
    projectId = count.toString();
  })
  afterEach(async ()=>{
    firebase.clearFirestoreData({ projectId })
    return Promise.all(firebase.apps().map(app => app.delete()))
  })
  describe(`authenticated`, function(){
    const uid = '123';
    beforeEach(function (){
      app = firebase.initializeTestApp({ projectId, 'auth': {uid} })
      return loadRules(WARD_NAME);
    })
    it(`succeeds on empty object`, async function(){
      const v:Test = {};

      await firebase.assertSucceeds(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`succeeds on string`, async function(){
      const v:Test = {str: 'abc'};
      await firebase.assertSucceeds(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a number`, async function(){
      const v:Test = {num: 123};
      await firebase.assertSucceeds(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a nested with missing optional property`, async function(){
      const v:Test = {sub: {first: 'Fire'}};
      await firebase.assertSucceeds(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a nested with optional property present`, async function(){
      const v:Test = {sub: {first: 'Fire', last: 'Ward'}};
      await firebase.assertSucceeds(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`fails on nothing but extra properties`, async function(){
      const v = {z: 123};
      await firebase.assertFails(app.firestore().collection(`example`).doc('x').set(v));
    })
    it(`fails on extra property with expected property`, async function(){
      const v:(Test & {z: number}) = {z: 123, str: '123'};
      await firebase.assertFails(app.firestore().collection(`example`).doc('x').set(v));
    })
  })
})