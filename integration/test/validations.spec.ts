/**
 * The logic of allow rules in combination with type checks
 */
 import firebaseTesting = require('@firebase/rules-unit-testing');
 import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
 import {getRules} from '../util/rules';
 
import {Pilot} from '../wards/validations';


const WARD_NAME = 'validations';
let firestore: ReturnType<RulesTestContext['firestore']>;
let app: RulesTestContext;
let testEnv: RulesTestEnvironment;

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}})
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

    beforeEach(function (){
      app = testEnv.authenticatedContext(uid, {});
      firestore = app.firestore();
    });

    it(`succeeds saving a validated record`, async function() {
      
      const p:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`pilots`).doc(uid).set(p));
    });

    it(`fails saving a with data validation`, async function() {
      
      const p:Pilot = {
        experience: 3,
        name: 'Capt. Rex Kramer'
      }
      await firebaseTesting.assertFails(firestore.collection(`pilots`).doc(uid).set(p));
    });

    it(`fails saving a with data and prev comparison validation`, async function() {
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`pilots`).doc(uid).set(p1));
      const p2:Pilot = {
        experience: 4,
        name: 'Capt. Rex Kramer'
      }
      await firebaseTesting.assertFails(firestore.collection(`pilots`).doc(uid).set(p2));
    });

    it(`succeeds saving a with data and prev comparison validation`, async function() {
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Kramer'
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`pilots`).doc(uid).set(p1));
      const p2:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`pilots`).doc(uid).set(p2));
    });

    it(`fails for Otto`, async function() {
      
      const p1:Pilot = {
        experience: 5,
        name: 'Otto'
      }
      await firebaseTesting.assertFails(firestore.collection(`pilots`).doc(uid).set(p1));
    });

    it(`Capt. Clarence Oveur fails to return`, async function() {
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Clarence Oveur'
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`pilots`).doc(uid).set(p1));
    });
   
  });

});