
import {expect} from 'chai';
import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';

import {} from '../wards/expressions';


const WARD_NAME = 'expressions';

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

    it(`succeeds write for 1==1 || 2==2`, async function() {
      
      const data = {}
      await firebaseTesting.assertSucceeds(firestore.collection(`a`).doc(uid).set(data));
    });

    it(`fails write for impossible "a"!="a" || "b"!="b"`, async function() {

      const data = {}
      await firebaseTesting.assertFails(firestore.collection(`b`).doc(uid).set(data));
    });
    
    it(`succeeds write for compound rule`, async function() {

      const data = {test: 'f'}
      await firebaseTesting.assertSucceeds(firestore.collection(`c`).doc(uid).set(data));

    });

    it(`fails write for compound rule`, async function() {

      const data = {test: 'g'}
      await firebaseTesting.assertFails(firestore.collection(`c`).doc(uid).set(data));

    });

    it(`succeeds write for indexed element [1,2,3][1]==2`, async function() {

      const data = {test: 'f'}
      await firebaseTesting.assertSucceeds(firestore.collection(`d`).doc(uid).set(data));

    });

    it(`succeeds write for a function call`, async function() {

      const data = {test: 'f'}
      await firebaseTesting.assertSucceeds(firestore.collection(`e`).doc('hello').set(data));

    });

    it(`succeeds write for a regex match`, async function() {

      const data = {test: 'f'}
      await firebaseTesting.assertSucceeds(firestore.collection(`f`).doc('hello').set(data));

    });

  });
  
});