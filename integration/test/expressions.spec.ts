
import {expect} from 'chai';
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {} from '../wards/expressions';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'expressions';
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
      return loadRules(WARD_NAME, app);
    })
    it(`succeeds write for 1==1 || 2==2`, async function(){
      
      const data = {}
      await firebase.assertSucceeds(app.firestore().collection(`a`).doc(uid).set(data));
    })
    // it(`fails write for impossible "a"!="a" || "b"!="b"`, async function(){
      
    //   const data = {}
    //   await firebase.assertFails(app.firestore().collection(`b`).doc(uid).set(data));
    // })
    // it(`succeeds write for compound rule`, async function(){
      
    //   const data = {test: 'f'}
    //   await firebase.assertSucceeds(app.firestore().collection(`c`).doc(uid).set(data));

    // })
    // it(`fails write for compound rule`, async function(){
      
    //   const data = {test: 'g'}
    //   await firebase.assertFails(app.firestore().collection(`c`).doc(uid).set(data));

    // })
    // it(`succeeds write for indexed element [1,2,3][1]==2`, async function(){
      
    //   const data = {test: 'f'}
    //   await firebase.assertSucceeds(app.firestore().collection(`d`).doc(uid).set(data));

    // })
    // it(`succeeds write for a function call`, async function(){
      
    //   const data = {test: 'f'}
    //   await firebase.assertSucceeds(app.firestore().collection(`e`).doc('hello').set(data));

    // })
    // it(`succeeds write for a regex match`, async function(){
      
    //   const data = {test: 'f'}
    //   await firebase.assertSucceeds(app.firestore().collection(`f`).doc('hello').set(data));

    // })
  })
  
})
