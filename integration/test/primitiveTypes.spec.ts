/**
 * The logic of allow rules in combination with type checks
 */
import {GeoPoint} from 'firebase/firestore'
import firebaseTesting = require('@firebase/rules-unit-testing');
import {RulesTestEnvironment, RulesTestContext} from '@firebase/rules-unit-testing'
import {getRules} from '../util/rules';
 
import {ListTest, OptListTest, MapTest, LitTest, WardTimestamp, TimestampTest, isTimestamp, GeoTest, WardGeoPoint, isGeoPoint, AnyTest, FirewardInput, FirewardOutput, TimestampNullTest, QuotedTest} from '../wards/primitiveTypes';

import { expect } from 'chai';

const WARD_NAME = 'primitiveTypes';

let app: RulesTestContext;
let testEnv: RulesTestEnvironment
let firestore: ReturnType<RulesTestContext['firestore']>

const projectId = WARD_NAME.toLowerCase();
const uid = '123';

before(async function() {
  const rules = getRules(WARD_NAME);
  testEnv = await firebaseTesting.initializeTestEnvironment({projectId, firestore: {rules}});

  app = testEnv.authenticatedContext(uid, {});
  firestore = app.firestore();
});

describe(WARD_NAME, function() {
  let count = 0;
  beforeEach(async function() {
    count++;
  });

  afterEach(async ()=>{
    testEnv.clearFirestore();
  });

  describe(`authenticated`, function() {
    
    beforeEach(function () {
      
    });

    it(`succeeds saving a list primitive field`, async function() {
      const a: ListTest = { test: 'abcabcabcabcabcabcabcabcabcabcabcabc'.split('') }
      await firebaseTesting.assertSucceeds(firestore.collection(`list`).doc(uid).set(a));
      const b: ListTest = { test: 'bb'.split('') }
      await firebaseTesting.assertSucceeds(firestore.collection(`list`).doc(uid).set(b));
    });

    it(`succeeds creating an optional list primitive field`, async function() {
      
      const a: OptListTest = {  }
      await firebaseTesting.assertSucceeds(firestore.collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { test: 'bb'.split('') }
      await firebaseTesting.assertSucceeds(firestore.collection(`olist`).doc(uid).set(b));
    });

    it(`succeeds uncreating an optional list primitive field`, async function() {
      
      const a: OptListTest = { test: 'bb'.split('') }
      await firebaseTesting.assertSucceeds(firestore.collection(`olist`).doc(uid).set(a));
      const b: OptListTest = { }
      await firebaseTesting.assertSucceeds(firestore.collection(`olist`).doc(uid).set(b));
    });

    it(`succeeds saving a map primitive field`, async function() {
      
      const a: MapTest = { test: {a:1,b:'2', c: true} }
      await firebaseTesting.assertSucceeds(firestore.collection(`map`).doc(uid).set(a));
      const b: MapTest = { test: {a:1.1,b:'2.7', c: [1,2,3]} }
      await firebaseTesting.assertSucceeds(firestore.collection(`map`).doc(uid).set(b));
    });

    it(`succeeds saving number, string and boolean into any type`, async function() {
      const x: AnyTest = {
        test: 123
      }
      await firebaseTesting.assertSucceeds(firestore.collection(`any`).doc(uid).set(x));
      x.test = true;
      await firebaseTesting.assertSucceeds(firestore.collection(`any`).doc(uid).set(x));
      x.test = "fireward is like an impenetrable wall";
      await firebaseTesting.assertSucceeds(firestore.collection(`any`).doc(uid).set(x));
      const xx = await firestore.collection(`any`).doc(uid).get();
      if (x.test!==xx.data()?.test) throw new Error(`Any item did not properly save.`)
    })

    describe(`Literal Types`, function() {
      
      let x: LitTest;

      beforeEach(()=>{
        x = {
          'numTest': 123,
          'boolTest': false,
          'mixTest': 123,
          'strTest': 'you'
        }
      });

      it(`succeeds saving a map of literal types`, async function() {
        await firebaseTesting.assertSucceeds(firestore.collection(`literal`).doc(uid).set(x));
        x.numTest = 234;
        x.mixTest = "hello";
        x.strTest = 'me';
        await firebaseTesting.assertSucceeds(firestore.collection(`literal`).doc(uid).set(x));
      });
      
      it(`fails saving an unmatched number`, async function() {
        // @ts-ignore
        x.numTest = 111;
        await firebaseTesting.assertFails(firestore.collection(`literal`).doc(uid).set(x));
      });

      it(`fails saving an unmatched string`, async function() {
        // @ts-ignore
        x.strTest = 'x';
        await firebaseTesting.assertFails(firestore.collection(`literal`).doc(uid).set(x));
      });

      it(`fails saving an unmatched boolean`, async function() {
        // @ts-ignore
        x.boolTest = true;
        await firebaseTesting.assertFails(firestore.collection(`literal`).doc(uid).set(x));
      });
      
    });

    describe(`WardTimestamp`, function() {

      it(`typechecks by isTimestamp`, async function() {
        const x: TimestampTest = {
          test: new Date()
        };
        await firebaseTesting.assertSucceeds(firestore.collection(`time`).doc(uid).set(x));
        const xx = await firestore.collection('time').doc(uid).get();
        const data = xx.data();
        const y: unknown = data && data.test;
        if (!isTimestamp(y)) throw new Error(`Expected a WardTimestamp but got ` + JSON.stringify(y, null, '  '))
        if (!y.nanoseconds) throw new Error(`No nanoseconds? What a strange coincidence. Re-run the test.`)
      });

    });

    describe(`WardGeoPoint`, function() {
      it(`typechecks a geopoint`, async function() {
        const x: GeoTest = {
          test: new GeoPoint(1, 1)
        };
        if (!isGeoPoint(x.test)) throw new Error(`A correct GeoPoint didn't typecheck`);
        await firebaseTesting.assertSucceeds(firestore.collection(`geo`).doc(uid).set(x));
        const xx = await firestore.collection('geo').doc(uid).get();
        const data = xx.data();
        const y: unknown = data && data.test;
        if (!isGeoPoint(y)) throw new Error(`Expected a WardGeoPoint but got ` + JSON.stringify(y, null, '  '))
        if (y.latitude!=x.test.latitude) throw new Error(`Geopoint.latitude did not save correctly`)
      });

    });

    describe(`Quoted property names`, function() {
      it(`reads/writes Japanese, as well as non-word characters`, async function() {
        const x: QuotedTest = {
          'ハロー・ワールド': 'hello world',
          'abc': {
            '..-': {']': 123}
          }
        };
        await firebaseTesting.assertSucceeds(firestore.collection('quoted').doc(uid).set(x));
        const xx = await firestore.collection('quoted').doc(uid).get();
        expect(xx.data()).eql(x);
      });
    });
    
  })
})
