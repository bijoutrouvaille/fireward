export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardTimestamp)=>boolean, toMillis: ()=>number, valueOf: ()=>string};
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean, toJSON: ()=>{latitude: number, longitude: number} }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };
export type FirewardOutput = /** what you get from DB */ { timestamp: WardTimestamp|null; number: number; };
export type FirewardInput = /** what you send to DB */ { timestamp: WardTimestamp|Date|WardFieldValue; number: number|WardFieldValue; };
export type FirewardTypes = FirewardInput | FirewardOutput;

export type ListTest<Types extends FirewardTypes = FirewardTypes> = {
  test: string[]
}
export type OptListTest<Types extends FirewardTypes = FirewardTypes> = {
  test?: string[]
}
export type MapTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Record<string, unknown>
}
export type LitTest<Types extends FirewardTypes = FirewardTypes> = {
  strTest: 'me' | 'you'
  numTest: 123 | 234
  boolTest: false
  mixTest: 'hello' | 123 | true
}
export type TimestampTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Types['timestamp']
}
export type TimestampNullTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Types['timestamp'] | null
}
export type AnyTest<Types extends FirewardTypes = FirewardTypes> = {
  test: any
}
export type GeoTest<Types extends FirewardTypes = FirewardTypes> = {
  test: WardGeoPoint
}
export type UTF8Test<Types extends FirewardTypes = FirewardTypes> = {

}
export type QuotedTest<Types extends FirewardTypes = FirewardTypes> = {
  "ハロー・ワールド": string
  "abc": {
    "..-": {
      "]": Types['number']
    }
  }
}








