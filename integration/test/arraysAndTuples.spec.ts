/**
 * The logic of allow rules in combination with type checks
 */
 import firebaseTesting = require('@firebase/rules-unit-testing');
 import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
 import {getRules} from '../util/rules';
 
import {Tuple1, A, B, Booking} from '../wards/arraysAndTuples';

const WARD_NAME = 'arraysAndTuples';

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment;
let firestore: ReturnType<RulesTestContext['firestore']>;


before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});
});

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

    it(`succeeds saving a tuple`, async function() {
      let a:A = {
        test: ['f', 3, 'hello', {a: 1}]
      }

      await firebaseTesting.assertSucceeds(firestore.collection(`a`).doc(uid).set(a));
    });

    it(`fails saving a tuple with optionals that's too short`, async function() {
      let x = {
        test: [1]
      }

      await firebaseTesting.assertFails(firestore.collection(`b`).doc(uid).set(x));
    });

    it(`succeeds saving a tuple with optionals that's min len`, async function() {
      let x:B = {
        test: [1,2]
      }

      await firebaseTesting.assertSucceeds(firestore.collection(`b`).doc(uid).set(x));
    });

    it(`succeeds saving a tuple with optionals that's full len`, async function() {
      let x:B = {
        test: [1,2,3,4]
      }

      await firebaseTesting.assertSucceeds(firestore.collection(`b`).doc(uid).set(x));
    });

    it(`fails saving a tuple with optionals that's too long`, async function() {
      let x = {
        test: [1,2,3,4,5]
      }

      await firebaseTesting.assertFails(firestore.collection(`b`).doc(uid).set(x));
    });

    it(`saves and updates all custom type rule; issue #18`, async function() {
      let x: Booking = {
        sessions: [{dayOfWeek: 2}]
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`book`).doc(uid).set(x));
      x.sessions.push({dayOfWeek: 33})
      await firebaseTesting.assertSucceeds(firestore.collection(`book`).doc(uid).set(x));
      let y = {
        sessions: [{dayOfWeek: 2},{dayOfWeek: 22},{dayOfWeek: 222}]
      }

      await firebaseTesting.assertFails(firestore.collection(`book`).doc(uid).set(y));
    });

  });

});