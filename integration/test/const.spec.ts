/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {StrTest, IntTest, FloatTest, MapTest, BoolTest, OptTest} from '../wards/const';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'const';
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
    it(`fails updating of const str`, async function(){
      
      const a: StrTest = { test: '123', name: "Ward" }
      await app.firestore().collection(`str`).doc(uid).set(a);
      const b: StrTest = { test: '234', name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`str`).doc(uid).set(b));
    });
    it(`succeeds update if const str is the same`, async function(){
      
      const a: StrTest = { test: '123', name: "Ward" }
      await app.firestore().collection(`str`).doc(uid).set(a);
      const b: StrTest = { test: '123', name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`str`).doc(uid).set(b));
    });
    it(`fails updating of const int`, async function(){
      
      const a: IntTest = { test: 123, name: "Ward" }
      await app.firestore().collection(`int`).doc(uid).set(a);
      const b: IntTest = { test: 234, name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`int`).doc(uid).set(b));
    });
    it(`succeeds update if const int is the same`, async function(){
      
      const a: IntTest = { test: 123, name: "Ward" }
      await app.firestore().collection(`int`).doc(uid).set(a);
      const b: IntTest = { test: 123, name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`int`).doc(uid).set(b));
    });
    it(`fails updating of const float`, async function(){
      
      const a: FloatTest = { test: 123.5, name: "Ward" }
      await app.firestore().collection(`float`).doc(uid).set(a);
      const b: FloatTest = { test: 234.1, name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`float`).doc(uid).set(b));
    });
    it(`succeeds update if const float is the same`, async function(){
      
      const a: FloatTest = { test: 123.5, name: "Ward" }
      await app.firestore().collection(`float`).doc(uid).set(a);
      const b: FloatTest = { test: 123.5, name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`float`).doc(uid).set(b));
    });
    it(`fails updating of const bool`, async function(){
      
      const a: BoolTest = { test: true, name: "Ward" }
      await app.firestore().collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: false, name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`bool`).doc(uid).set(b));
    });
    it(`succeeds update if const bool true is the same`, async function(){
      
      const a: BoolTest = { test: true, name: "Ward" }
      await app.firestore().collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: true, name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`bool`).doc(uid).set(b));
    });
    it(`succeeds update if const bool false is the same`, async function(){
      
      const a: BoolTest = { test: false, name: "Ward" }
      await app.firestore().collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: false, name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`bool`).doc(uid).set(b));
    });
    it(`fails updating of const map`, async function(){
      
      const a: MapTest = { test: {a:1}, name: "Ward" }
      await app.firestore().collection(`map`).doc(uid).set(a);
      const b: MapTest = { test: {a:2}, name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`map`).doc(uid).set(b));
    });
    it(`succeeds update if const map is the same`, async function(){
      
      const a: MapTest = { test: {a:1}, name: "Ward" }
      await app.firestore().collection(`map`).doc(uid).set(a);
      const b: MapTest = { test: {a:1}, name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`map`).doc(uid).set(b));
    });
    it(`fails updating of optional const string`, async function(){
      
      const a: OptTest = { test: '123', name: "Ward" }
      await app.firestore().collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '234', name: "Ward" }
      await firebase.assertFails(app.firestore().collection(`opt`).doc(uid).set(b));
    });
    it(`succeeds update if const opt is the same`, async function(){
      
      const a: OptTest = { test: '123', name: "Ward" }
      await app.firestore().collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '123', name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`opt`).doc(uid).set(b));
    });
    it(`succeeds update if const opt was missing the first time`, async function(){
      
      const a: OptTest = { name: "Ward" }
      await app.firestore().collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '123', name: "Fire" }
      await firebase.assertSucceeds(app.firestore().collection(`opt`).doc(uid).set(b));
    });
    
  })
})
