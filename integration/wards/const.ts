export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardTimestamp)=>boolean, toMillis: ()=>number, valueOf: ()=>string};
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean, toJSON: ()=>{latitude: number, longitude: number} }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };
export type FirewardOutput = /** what you get from DB */ { timestamp: WardTimestamp|null; number: number; };
export type FirewardInput = /** what you send to DB */ { timestamp: WardTimestamp|Date|WardFieldValue; number: number|WardFieldValue; };
export type FirewardTypes = FirewardInput | FirewardOutput;

export type StrTest<Types extends FirewardTypes = FirewardTypes> = {
  test: string
  name: string
}
export type IntTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Types['number']
  name: string
}
export type FloatTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Types['number']
  name: string
}
export type BoolTest<Types extends FirewardTypes = FirewardTypes> = {
  test: boolean
  name: string
}
export type MapTest<Types extends FirewardTypes = FirewardTypes> = {
  test: Record<string, unknown>
  name: string
}
export type OptTest<Types extends FirewardTypes = FirewardTypes> = {
  test?: string
  name: string
}





