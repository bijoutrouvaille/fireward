/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'paths';
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
    it(`allows a complex exists condition`, async function() {
      await app.firestore().collection('refz').doc('qq').set({})
      await firebase.assertSucceeds(app.firestore().collection('exists').doc('refz').set({}))
    })
    it(`allows read when condition uses function with custom let bindings`, async function(){
      const ref = app.firestore().collection('testTrueFunc').doc('x');
      await ref.set({});
      await firebase.assertSucceeds(ref.get())
    })
    it(`denies read when condition uses function with custom let bindings`, async function(){
      const ref = app.firestore().collection('testFalseFunc').doc('x');
      await ref.set({});
      await firebase.assertFails(ref.get())
    })
    // it(`denies write of a directive with 'false' body`, async function() {
    //   await firebase.assertFails(app.firestore().collection('dir').doc(uid).set({}))
    // })
  })
})

