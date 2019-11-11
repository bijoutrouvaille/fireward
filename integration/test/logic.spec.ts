/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {User, Name} from '../wards/logic';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'logic';
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
    it(`succeeds update`, async function(){
      
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      await app.firestore().collection(`users`).doc(uid).set(user);
      const u2: User = { name: 'FireWard' }
      await firebase.assertSucceeds(app.firestore().collection(`users`).doc(uid).set(u2));
    });
    it(`fails update if auth.uid!=param.uid`, async function(){
      
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      await app.firestore().collection(`users`).doc('x').set(user);
      const u2: User = { name: 'FireWard' }
      await firebase.assertFails(app.firestore().collection(`users`).doc('x').set(u2));
    });

    it(`fails delete because delete rule is not specified`, async function(){
      const user: User = { name: 'The Warden'};
      await app.firestore().collection(`users`).doc(uid).set(user);
      await firebase.assertFails(app.firestore().collection('users').doc(uid).delete())
    })
    it(`allows read of a directive without a body`, async function() {
      await firebase.assertSucceeds(app.firestore().collection('dir').doc(uid).get())
    })
    it(`denies write of a directive with 'false' body`, async function() {
      await firebase.assertFails(app.firestore().collection('dir').doc(uid).set({}))
    })
  })
})
