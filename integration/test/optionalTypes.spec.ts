
import {expect} from 'chai';
import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';
import {OptionalTypesExample as Test} from '../wards/optionalTypes';

const WARD_NAME = 'optionalTypes';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment
let firestore: ReturnType<RulesTestContext['firestore']>

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

before(async function() {

  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});
  

  app = testEnv.authenticatedContext(uid, {});
  firestore = app.firestore();
})

describe(WARD_NAME, function() {
  let count = 0;
  beforeEach(async function() {
    count++;
  })
  afterEach(async ()=>{
    testEnv.clearFirestore();
  })
  describe(`authenticated`, function() {
    
    beforeEach(function (){
      
    })
    it(`succeeds on empty object`, async function() {
      const v:Test = {};

      await firebaseTesting.assertSucceeds(firestore.collection(`example`).doc('x').set(v));
    })
    it(`succeeds on string`, async function() {
      const v:Test = {str: 'abc'};
      await firebaseTesting.assertSucceeds(firestore.collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a number`, async function() {
      const v:Test = {num: 123};
      await firebaseTesting.assertSucceeds(firestore.collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a nested with missing optional property`, async function() {
      const v:Test = {sub: {first: 'Fire'}};
      await firebaseTesting.assertSucceeds(firestore.collection(`example`).doc('x').set(v));
    })
    it(`succeeds on a nested with optional property present`, async function() {
      const v:Test = {sub: {first: 'Fire', last: 'Ward'}};
      await firebaseTesting.assertSucceeds(firestore.collection(`example`).doc('x').set(v));
    })
    it(`fails on nothing but extra properties`, async function() {
      const v = {z: 123};
      await firebaseTesting.assertFails(firestore.collection(`example`).doc('x').set(v));
    })
    it(`fails on extra property with expected property`, async function() {
      const v:(Test & {z: number}) = {z: 123, str: '123'};
      await firebaseTesting.assertFails(firestore.collection(`example`).doc('x').set(v));
    })
  })
})
