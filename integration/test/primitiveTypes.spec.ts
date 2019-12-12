/**
 * The logic of allow rules in combination with type checks
 */
import firebase = require('@firebase/testing');
import {loadRules} from '../util/rules';
import {ListTest, OptListTest, MapTest, LitTest, WardTimestamp, TimestampTest, isTimestamp} from '../wards/primitiveTypes';
import {isEmulatorReady} from './../util/emulator'

const WARD_NAME = 'primitiveTypes';
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
    it(`succeeds saving a list primitive field`, async function(){
      
      const a: ListTest = { test: 'abcabcabcabcabcabcabcabcabcabcabcabc'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`list`).doc(uid).set(a));
      const b: ListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`list`).doc(uid).set(b));
    });
    it(`succeeds creating an optional list primitive field`, async function(){
      
      const a: OptListTest = {  }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(b));
    });
    it(`succeeds uncreating an optional list primitive field`, async function(){
      
      const a: OptListTest = { test: 'bb'.split('') }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { }
      await firebase.assertSucceeds(app.firestore().collection(`olist`).doc(uid).set(b));
    });
    it(`succeeds saving a map primitive field`, async function(){
      
      const a: MapTest = { test: {a:1,b:'2', c: true} }
      await firebase.assertSucceeds(app.firestore().collection(`map`).doc(uid).set(a));
      const b: MapTest = { test: {a:1.1,b:'2.7', c: [1,2,3]} }
      await firebase.assertSucceeds(app.firestore().collection(`map`).doc(uid).set(b));
    });

    describe(`Literal Types`, function() {
      let x: LitTest;
      beforeEach(()=>{
        x = {
          'numTest': 123,
          'boolTest': false,
          'mixTest': 123,
          'strTest': 'you'
        }
      })
      it(`succeeds saving a map of literal types`, async function(){
        await firebase.assertSucceeds(app.firestore().collection(`literal`).doc(uid).set(x));
        x.numTest = 234;
        x.mixTest = "hello";
        x.strTest = 'me';
        await firebase.assertSucceeds(app.firestore().collection(`literal`).doc(uid).set(x));
      })
      
      it(`fails saving an unmatched number`, async function(){
        // @ts-ignore
        x.numTest = 111;
        await firebase.assertFails(app.firestore().collection(`literal`).doc(uid).set(x));
      })
      it(`fails saving an unmatched string`, async function(){
        // @ts-ignore
        x.strTest = 'x';
        await firebase.assertFails(app.firestore().collection(`literal`).doc(uid).set(x));
      })
      it(`fails saving an unmatched boolean`, async function(){
        // @ts-ignore
        x.boolTest = true;
        await firebase.assertFails(app.firestore().collection(`literal`).doc(uid).set(x));
      })
      
    })
    describe(`WardTimestamp`, function(){
      it(`typechecks by isTimestamp`, async function(){
        const x: TimestampTest = {
          test: new Date()
        };
        await firebase.assertSucceeds(app.firestore().collection(`time`).doc(uid).set(x));
        const xx = await app.firestore().collection('time').doc(uid).get();
        const data = xx.data();
        const y: unknown = data && data.test;
        if (!isTimestamp(y)) throw new Error(`Expected a WardTimestamp but got ` + JSON.stringify(y, null, '  '))
        if (!y.nanoseconds) throw new Error(`No nanoseconds? What a strange coincidence. Re-run the test.`)
      })
    })
  })
})
