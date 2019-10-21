
import {expect} from 'chai';
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {User, Name} from '../wards/simple';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'simple';
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
    })
  })
  describe(`un-authenticated`, function(){
    beforeEach(function (){
      app = firebase.initializeTestApp({ projectId })
      return loadRules(WARD_NAME, app);
    })
    it(`succeeds read`, function(){
      return firebase.assertSucceeds(app.firestore().collection('users').doc('123').get())
    })
    it(`succeeds create`, async function(){
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      return app.firestore().collection(`users`).add(user)
    })
    it(`fails update`, async function(){
      const user: User = { name: {first: 'Fire', last: "Ward"} }
      const ref = await app.firestore().collection(`users`).add(user);
      const u2: User = { name: 'FireWard' }
      await firebase.assertFails(app.firestore().collection(`users`).doc(ref.id).set(u2));
    })
  })
})
