export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardFieldValue)=>boolean, toMillis: ()=>number};
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };

export type ListTest = {
  test: string[]
}
export type OptListTest = {
  test?: string[]
}
export type MapTest = {
  test: Record<string, unknown>
}
export type LitTest = {
  strTest: 'me' | 'you'
  numTest: 123 | 234
  boolTest: false
  mixTest: 'hello' | 123 | true
}
export type TimestampTest = {
  test: null|Date|WardTimestamp|WardFieldValue
}
export type AnyTest = {
  test: any
}
export type GeoTest = {
  test: WardGeoPoint
}
export type UTF8Test = {

}
export type QuotedTest = {
  "ハロー・ワールド": string
  "abc": {
    "..-": {
      "]": number
    }
  }
}







