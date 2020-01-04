/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {Tuple1, A, B} from '../wards/arraysAndTuples';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'arraysAndTuples';
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
    it(`succeeds saving a tuple`, async function(){
      let a:A = {
        test: ['f', 3, 'hello', {a: 1}]
      }
      await firebase.assertSucceeds(app.firestore().collection(`a`).doc(uid).set(a));
    });

    it(`fails saving a tuple with optionals that's too short`, async function(){
      let x = {
        test: [1]
      }
      await firebase.assertFails(app.firestore().collection(`b`).doc(uid).set(x));
    })
    it(`succeeds saving a tuple with optionals that's min len`, async function(){
      let x:B = {
        test: [1,2]
      }
      await firebase.assertSucceeds(app.firestore().collection(`b`).doc(uid).set(x));
    })
    it(`succeeds saving a tuple with optionals that's full len`, async function(){
      let x:B = {
        test: [1,2,3,4]
      }
      await firebase.assertSucceeds(app.firestore().collection(`b`).doc(uid).set(x));
    })
    it(`fails saving a tuple with optionals that's too long`, async function(){
      let x = {
        test: [1,2,3,4,5]
      }
      await firebase.assertFails(app.firestore().collection(`b`).doc(uid).set(x));
    })
    
    
    
   
  })
})
