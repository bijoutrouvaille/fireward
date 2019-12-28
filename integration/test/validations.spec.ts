/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {Pilot} from '../wards/validations';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'validations';
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
    it(`succeeds saving a validated record`, async function(){
      
      const p:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebase.assertSucceeds(app.firestore().collection(`pilots`).doc(uid).set(p));
    });
    it(`fails saving a with data validation`, async function(){
      
      const p:Pilot = {
        experience: 3,
        name: 'Capt. Rex Kramer'
      }
      await firebase.assertFails(app.firestore().collection(`pilots`).doc(uid).set(p));
    });
    it(`fails saving a with data and prev comparison validation`, async function(){
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebase.assertSucceeds(app.firestore().collection(`pilots`).doc(uid).set(p1));
      const p2:Pilot = {
        experience: 4,
        name: 'Capt. Rex Kramer'
      }
      await firebase.assertFails(app.firestore().collection(`pilots`).doc(uid).set(p2));
    });
    it(`succeeds saving a with data and prev comparison validation`, async function(){
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Kramer'
      }
      await firebase.assertSucceeds(app.firestore().collection(`pilots`).doc(uid).set(p1));
      const p2:Pilot = {
        experience: 5,
        name: 'Capt. Rex Kramer'
      }
      await firebase.assertSucceeds(app.firestore().collection(`pilots`).doc(uid).set(p2));
    });
    it(`fails for Otto`, async function(){
      
      const p1:Pilot = {
        experience: 5,
        name: 'Otto'
      }
      await firebase.assertFails(app.firestore().collection(`pilots`).doc(uid).set(p1));
    });
    it(`Capt. Clarence Oveur fails to return`, async function(){
      
      const p1:Pilot = {
        experience: 5,
        name: 'Capt. Clarence Oveur'
      }
      await firebase.assertSucceeds(app.firestore().collection(`pilots`).doc(uid).set(p1));
    });
    
    
   
  })
})
