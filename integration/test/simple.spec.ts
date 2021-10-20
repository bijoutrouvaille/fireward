
import {expect} from 'chai';
import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';

import {User, Name} from '../wards/simple';

const WARD_NAME = 'simple';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment;
let firestore: ReturnType<RulesTestContext['firestore']>;

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}})
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
    beforeEach(function() {
      app = testEnv.authenticatedContext(uid, {});
      firestore = app.firestore()
    });

    it(`succeeds update`, async function() {
      
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      await firestore.collection(`users`).doc(uid).set(user);
      const u2: User = { name: 'FireWard' }
      await firebaseTesting.assertSucceeds(firestore.collection(`users`).doc(uid).set(u2));
    });

  });

  describe(`un-authenticated`, function() {

    beforeEach(function (){
      app = testEnv.unauthenticatedContext();
      firestore = app.firestore()
    });

    it(`succeeds read`, function() {
      return firebaseTesting.assertSucceeds(firestore.collection('users').doc('123').get())
    });

    it(`succeeds create`, async function() {
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      return firestore.collection(`users`).add(user)
    });

    it(`fails update`, async function() {
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      const ref = await firestore.collection(`users`).add(user);
      const u2: User = { name: 'FireWard' }
      await firebaseTesting.assertFails(firestore.collection(`users`).doc(ref.id).set(u2));
    });

  });

});
