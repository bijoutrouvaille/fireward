export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardTimestamp)=>boolean, toMillis: ()=>number, valueOf: ()=>string};
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean, toJSON: ()=>{latitude: number, longitude: number} }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };
export type FirewardOutput = /** what you get from DB */ { timestamp: WardTimestamp|null; number: number; };
export type FirewardInput = /** what you send to DB */ { timestamp: WardTimestamp|Date|WardFieldValue; number: number|WardFieldValue; };
export type FirewardTypes = FirewardInput | FirewardOutput;

export type Tuple1<Types extends FirewardTypes = FirewardTypes> = [string, 3, 'hello', {
  a: 1
}?]
export type A<Types extends FirewardTypes = FirewardTypes> = {
  test: Tuple1<Types>
}

export type B<Types extends FirewardTypes = FirewardTypes> = {
  test: [Types['number'], Types['number'], Types['number']?, Types['number']?]
}

export type Test1<Types extends FirewardTypes = FirewardTypes> = {
  a: 1
}
export type C<Types extends FirewardTypes = FirewardTypes> = {
  test: Test1<Types>[]
}

export type Session<Types extends FirewardTypes = FirewardTypes> = {
  dayOfWeek: Types['number']
}
export type Booking<Types extends FirewardTypes = FirewardTypes> = {
  sessions: [Session<Types>, Session<Types>?]
}
