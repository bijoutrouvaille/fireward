export type ArrayMax1<T> = [T?]

export type ArrayMax2<T> = [T?, T?]

export type ArrayMax3<T> = [T?, T?, T?]

export type ArrayMax4<T> = [T?, T?, T?, T?]

export type ArrayMax5<T> = [T?, T?, T?, T?, T?]

export type ArrayMax6<T> = [T?, T?, T?, T?, T?, T?]

export type ArrayMax7<T> = [T?, T?, T?, T?, T?, T?, T?]

export type ArrayMax8<T> = [T?, T?, T?, T?, T?, T?, T?, T?]

export type ArrayMax9<T> = [T?, T?, T?, T?, T?, T?, T?, T?, T?]

export type ArrayMax10<T> = [T?, T?, T?, T?, T?, T?, T?, T?, T?, T?]

export type ArrayMax11<T> = [T?, T?, T?, T?, T?, T?, T?, T?, T?, T?, T?]

export type ArrayMax12<T> = [T?, T?, T?, T?, T?, T?, T?, T?, T?, T?, T?, T?]
export type WardTimestamp = {seconds: number, nanoseconds: number, toDate: ()=>Date, isEqual: (other: WardTimestamp)=>boolean, toMillis: ()=>number}
export function isTimestamp(v: any): v is WardTimestamp { return !!v && (typeof v=='object') && !!v.toDate && !!v.toMillis && (typeof v.nanoseconds=='number') && (typeof v.seconds=='number')};
export type SomeTuple<T> = ArrayMax1<T>  | ArrayMax2<T>  | ArrayMax3<T>  | ArrayMax4<T>  | ArrayMax5<T>  | ArrayMax6<T>  | ArrayMax7<T>  | ArrayMax8<T>  | ArrayMax9<T>  | ArrayMax10<T>  | ArrayMax11<T> 
export function toArrayMax<T>(n: 1, arr:T[]):ArrayMax1<T>
export function toArrayMax<T>(n: 2, arr:T[]):ArrayMax2<T>
export function toArrayMax<T>(n: 3, arr:T[]):ArrayMax3<T>
export function toArrayMax<T>(n: 4, arr:T[]):ArrayMax4<T>
export function toArrayMax<T>(n: 5, arr:T[]):ArrayMax5<T>
export function toArrayMax<T>(n: 6, arr:T[]):ArrayMax6<T>
export function toArrayMax<T>(n: 7, arr:T[]):ArrayMax7<T>
export function toArrayMax<T>(n: 8, arr:T[]):ArrayMax8<T>
export function toArrayMax<T>(n: 9, arr:T[]):ArrayMax9<T>
export function toArrayMax<T>(n: 10, arr:T[]):ArrayMax10<T>
export function toArrayMax<T>(n: 11, arr:T[]):ArrayMax11<T>
export function toArrayMax(n:number, arr:any[]) { return arr.slice(0,n) }
export type WardGeoPoint = { latitude: number, longitude: number, isEqual: (other: WardGeoPoint)=>boolean }
export function isGeoPoint(v: any): v is WardGeoPoint {  return !!v && (typeof v=='object') && (typeof v.isEqual=='function')  && (typeof v.latitude=='number') && (typeof v.longitude=='number') };







