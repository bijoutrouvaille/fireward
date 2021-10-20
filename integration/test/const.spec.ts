/**
 * The logic of allow rules in combination with type checks
 */
// import firebase = require('firebase');
import * as firebase from 'firebase/app'
import {getFirestore} from 'firebase/firestore';
import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';
import {StrTest, IntTest, FloatTest, MapTest, BoolTest, OptTest} from '../wards/const';

const WARD_NAME = 'const';

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment
let firestore: ReturnType<RulesTestContext['firestore']>

before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});
})

describe(WARD_NAME, function() {
  
  let count = 0;
  
  beforeEach(async function() {
    count++;
  });

  afterEach(async () => {
    testEnv.clearFirestore();
  });

  describe(`authenticated`, function() {

    beforeEach(function() {
      app = testEnv.authenticatedContext(uid, {});
      firestore = app.firestore();
    });

    it(`fails updating of const str`, async function() {
      
      const a: StrTest = { test: '123', name: "Ward" }
      await firestore.collection(`str`).doc(uid).set(a);
      const b: StrTest = { test: '234', name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`str`).doc(uid).set(b));
    });

    it(`succeeds update if const str is the same`, async function() {
      
      const a: StrTest = { test: '123', name: "Ward" }
      await firestore.collection(`str`).doc(uid).set(a);
      const b: StrTest = { test: '123', name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`str`).doc(uid).set(b));
    });

    it(`fails updating of const int`, async function() {
      
      const a: IntTest = { test: 123, name: "Ward" }
      await firestore.collection(`int`).doc(uid).set(a);
      const b: IntTest = { test: 234, name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`int`).doc(uid).set(b));
    });

    it(`succeeds update if const int is the same`, async function() {
      
      const a: IntTest = { test: 123, name: "Ward" }
      await firestore.collection(`int`).doc(uid).set(a);
      const b: IntTest = { test: 123, name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`int`).doc(uid).set(b));
    });

    it(`fails updating of const float`, async function() {
      
      const a: FloatTest = { test: 123.5, name: "Ward" }
      await firestore.collection(`float`).doc(uid).set(a);
      const b: FloatTest = { test: 234.1, name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`float`).doc(uid).set(b));
    });

    it(`succeeds update if const float is the same`, async function() {
      
      const a: FloatTest = { test: 123.5, name: "Ward" }
      await firestore.collection(`float`).doc(uid).set(a);
      const b: FloatTest = { test: 123.5, name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`float`).doc(uid).set(b));
    });
    it(`fails updating of const bool`, async function() {
      
      const a: BoolTest = { test: true, name: "Ward" }
      await firestore.collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: false, name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`bool`).doc(uid).set(b));
    });

    it(`succeeds update if const bool true is the same`, async function() {
      
      const a: BoolTest = { test: true, name: "Ward" }
      await firestore.collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: true, name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`bool`).doc(uid).set(b));
    });

    it(`succeeds update if const bool false is the same`, async function() {
      
      const a: BoolTest = { test: false, name: "Ward" }
      await firestore.collection(`bool`).doc(uid).set(a);
      const b: BoolTest = { test: false, name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`bool`).doc(uid).set(b));
    });

    it(`fails updating of const map`, async function() {
      
      const a: MapTest = { test: {a:1}, name: "Ward" }
      await firestore.collection(`map`).doc(uid).set(a);
      const b: MapTest = { test: {a:2}, name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`map`).doc(uid).set(b));
    });

    it(`succeeds update if const map is the same`, async function() {
      
      const a: MapTest = { test: {a:1}, name: "Ward" }
      await firestore.collection(`map`).doc(uid).set(a);
      const b: MapTest = { test: {a:1}, name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`map`).doc(uid).set(b));
    });

    it(`fails updating of optional const string`, async function() {
      
      const a: OptTest = { test: '123', name: "Ward" }
      await firestore.collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '234', name: "Ward" }
      await firebaseTesting.assertFails(firestore.collection(`opt`).doc(uid).set(b));
    });

    it(`succeeds update if const opt is the same`, async function() {
      
      const a: OptTest = { test: '123', name: "Ward" }
      await firestore.collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '123', name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`opt`).doc(uid).set(b));
    });

    it(`succeeds update if const opt was missing the first time`, async function() {
      
      const a: OptTest = { name: "Ward" }
      await firestore.collection(`opt`).doc(uid).set(a);
      const b: OptTest = { test: '123', name: "Fire" }
      await firebaseTesting.assertSucceeds(firestore.collection(`opt`).doc(uid).set(b));
    });
    
  });
});
