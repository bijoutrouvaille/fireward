/**
 * The logic of allow rules in combination with type checks
 */

import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';

const WARD_NAME = 'paths';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment
let firestore: ReturnType<RulesTestContext['firestore']>

const projectId = WARD_NAME;
const uid = '123';


before(async function() {

  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});

  app = testEnv.authenticatedContext(uid, {});
  firestore = app.firestore();
})

describe(WARD_NAME, async function() {
  let count = 0;
  
  beforeEach(async function() {
    count++;
  });

  afterEach(async () => {
    testEnv.clearFirestore();
  });

  describe(`authenticated`, function() {
    beforeEach(function () {
    });

    it(`allows a complex exists condition`, async function() {
      await firestore.collection('refz').doc('qq').set({})
      await firebaseTesting.assertSucceeds(firestore.collection('exists').doc('refz').set({}))
    });

    it(`allows read when condition uses function with custom let bindings`, async function() {
      const ref = firestore.collection('testTrueFunc').doc('x');
      await ref.set({});
      await firebaseTesting.assertSucceeds(ref.get())
    });

    it(`denies read when condition uses function with custom let bindings`, async function() {
      const ref = firestore.collection('testFalseFunc').doc('x');
      await ref.set({});
      await firebaseTesting.assertFails(ref.get())
    });
    
    // it(`denies write of a directive with 'false' body`, async function() {
    //   await firebase.assertFails(firestore.collection('dir').doc(uid).set({}))
    // })
  })
})

