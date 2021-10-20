/**
 * The logic of allow rules in combination with type checks
 */
 import firebaseTesting = require('@firebase/rules-unit-testing');
 import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
 import {getRules} from '../util/rules';

import {User, Name} from '../wards/logic';

const WARD_NAME = 'logic';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment
let firestore: ReturnType<RulesTestContext['firestore']>

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});
})

describe(WARD_NAME, function() {
  let count = 0;
  beforeEach(async function() {
    count++;
  })
  afterEach(async () => {
    testEnv.clearFirestore();
  })
  describe(`authenticated`, function() {

    beforeEach(function (){
      app = testEnv.authenticatedContext(uid, {});
      firestore = app.firestore();
    });

    it(`succeeds update`, async function() {
      
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      await firestore.collection(`users`).doc(uid).set(user);
      const u2: User = { name: 'FireWard' }
      await firebaseTesting.assertSucceeds(firestore.collection(`users`).doc(uid).set(u2));
    });

    it(`fails update if auth.uid!=param.uid`, async function() {
      
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      await firestore.collection(`users`).doc('x').set(user);
      const u2: User = { name: 'FireWard' }
      await firebaseTesting.assertFails(firestore.collection(`users`).doc('x').set(u2));
    });

    it(`fails delete because delete rule is not specified`, async function() {
      const user: User = { name: 'The Warden'};
      await firestore.collection(`users`).doc(uid).set(user);
      await firebaseTesting.assertFails(firestore.collection('users').doc(uid).delete())
    });

    it(`allows read of a directive without a body`, async function() {
      await firebaseTesting.assertSucceeds(firestore.collection('dir').doc(uid).get())
    });

    it(`denies write of a directive with 'false' body`, async function() {
      await firebaseTesting.assertFails(firestore.collection('dir').doc(uid).set({}))
    });

  });
});
