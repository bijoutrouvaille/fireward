/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {ListTest, OptListTest, MapTest} from '../wards/primitiveTypes';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'primitiveTypes';
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
    it(`succeeds saving a list primitive field`, async function(){
      
      const a: ListTest = { test: 'abcabcabcabcabcabcabcabcabcabcabcabc'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`list`).doc(uid).set(a));
      const b: ListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`list`).doc(uid).set(b));
    });
    it(`succeeds creating an optional list primitive field`, async function(){
      
      const a: OptListTest = {  }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(b));
    });
    it(`succeeds uncreating an optional list primitive field`, async function(){
      
      const a: OptListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(b));
    });
    it(`succeeds saving a map primitive field`, async function(){
      
      const a: MapTest = { test: {a:1,b:'2', c: true} }
      await firebase.assertSucceeds(app.firestore().collection(`map`).doc(uid).set(a));
      const b: MapTest = { test: {a:1.1,b:'2.7', c: [1,2,3]} }
      await firebase.assertSucceeds(app.firestore().collection(`map`).doc(uid).set(b));
    });
  })
})
