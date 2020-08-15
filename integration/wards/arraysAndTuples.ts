export type WardFieldValue = { isEqual: (other: WardFieldValue) => boolean };
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardFieldValue)=>boolean, toMillis: ()=>number};
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };

export type Tuple1 = [string, 3, 'hello', {
  a: 1
}?]
export type A = {
  test: Tuple1
}

export type B = {
  test: [number, number, number?, number?]
}

export type Test1 = {
  a: 1
}
export type C = {
  test: Test1[]
}

export type Session = {
  dayOfWeek: number
}
export type Booking = {
  sessions: [Session, Session?]
}
