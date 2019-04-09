# Fireward

A successor to Firebase Bolt for writing Firestore rules. It also generates Typescript typings.

## Status

The project has just been released, so it's full of bugs, which the author will try to resolve soon after they are reported. Also it has very terrible error messages, so you'll pretty much have to guess at the problems. Usually, you'll just need to make sure you haven't misspelled any keywords, that there is an `=` in type definitions (`type X = {...`) and that the punctuation is properly placed.

## Discussion

https://groups.google.com/forum/#!forum/fireward

## Installation

#### Method 1: Binary

Download a [release](https://github.com/bijoutrouvaille/fireward/releases) binary and put it in your `PATH`.

#### Method 2: Compile Yourself

Download the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/), `cd` into the project directory and run `stack install`.

## Usage

```
fireward
  -i FILE      --input=FILE                          Input fireward file
  -o FILE      --output=FILE                         Output firestore.rules file
  -s FILE      --string=FILE                         Input string
  -l language  --lang=language, --language=language  Output language. One of: rules, typescript.
  -V           --version                             Print version
  -h           --help                                Show help
```

`--lang` is `rules` by default, and is, therefore, optional. If `-i` is not provided, `stdin` is read. If `-o` is not provided, output goes to `stdout`.

Example:

Generate rules: `fireward -i myrules.ward > firestore.rules`

Generate typescript definitions: `fireward -i myrules.ward --lang=typescript > MyTypings.ts`

## Rules Syntax

Fireward tries to keep things simple by mostly using syntax that already exists from the two languages it compiles to: Firestore Rules and Typescript. Basic steps are: 1. Define a type 2. Assign it to a route. The .ward file is essentially the Firestore rules augmented with Typescript types.

Fireward will wrap the code with the boilerplate 
```
service cloud.firestore {  
  match /databases/{database}/documents {
    ...
  }
}
```

### Basic Example

```
type User = {
  name: {first: string, last: string},
  friends: string[],
  age: int,
  contact?: Phone | Email
} 
type Phone = {number: int, country: int}
type Email = string


function isUser(userId) { request.auth!=null && request.auth.uid == userId; }

match /users/{userId} is User {    
  allow read: if isUser(userId);
  allow create: if isUser(userId);
  allow update: if isUser(userId);
  allow delete: if isUser("admin");
}

```

### Notes on Syntax

#### Types

Firestore rules' types don't map exactly to JavaScript, and Fireward handles them. In particular: 
- `int` and `float` map to Typescript `number`
- `timestamp` maps to `Date|{isEqual: (other: any)=>boolean}`. Snapshots will come as a `Date`, but you can additionally assign a server timestamp object (`firebase.firestore.FieldValue.serverTimestamp`) when writing to the database.
- `bool` in rules maps to TS `boolean`

#### Lists

Rules support lists, which transpile to arrays or tuples in TS. The syntax is `MyType[]` or `MyType[n]`. The second variant will transpile to a MyType tuple up to n in size. If n is 4 (`MyType[4]`), for example, then the result will be a 0,1,2,3 or 4-tuple. Check the top of the generated files for the exported types that represent it.

#### Optional Types and `null`

Unlike in Firebase Realitime Database, optional types differ from `null`s. Optional types are indicated with a `?` before the colon, e.g. `{phone?: string}`. _Warning_: this will allow you to define keys with value `undefined`, which Firestore may reject as an error. "Optional string" means that either the value is a string, or the key is absent from the object.

#### Punctuation

is important. The example above demonstrates it. Extra or missing marks will cause the file to fail compilation.

#### Route Matching, Conditions and Functions

For the exception of assigning a type to a route, the syntax is identical to the Firestore rule language syntax.

## Contributing

Contributions are welcome!

The project uses the stack tool and puts shortcuts into the makefile.

The project was born from an exercise in monadic programming (based on _Monads for Functional Programming_, Wadler, and _Thinking Functionally in Haskell_, chapter on Parsing), so the parser is written from scratch. It seems to be the same in concept as Parsec, but with less functionality.

## TODO

- Rewrite in actual Parsec to allow for better error messages
- Add comments

## License

MIT github.com/bijoutrouvaille
