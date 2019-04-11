# FireWard

A successor to Firebase Bolt for writing Firestore rules. It also generates TypeScript typings. The idea is to be able to add automatic type validation to routes.

## Status

The project has just been released, so it's full of bugs, which the author will try to resolve soon after they are reported. Also it has very terrible error messages, so you'll pretty much have to guess at the problems. Usually, you'll just need to make sure you haven't misspelled any keywords, that there is an `=` in type definitions (`type X = {...`) and that the punctuation is properly placed.

## Discussion

https://groups.google.com/forum/#!forum/fireward

## Installation

#### Method 1: Binary

Download a [release](https://github.com/bijoutrouvaille/fireward/releases) binary and put it in your `PATH`.

#### Method 2: Compile Yourself

Download the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/), `cd` into the project directory and run `stack install`. 

Stack will download and install the compiler and dependencies and put the `fireward` executable into the path given by `stack path --local-bin` command, which you may want to add to your `PATH`. If you have none of Haskell tooling installed, the build may take an unsupervised 30±20 minutes.

## Usage

```
fireward
  -i FILE      --input=FILE                          Input fireward file instead of stdin
  -o FILE      --output=FILE                         Output file instead of stdout
  -s STRING    --string=STRING                       Input string
  -l language  --lang=language, --language=language  Output language. One of: rules, typescript.
  -V           --version                             Print version
  -h           --help                                Show help
```

`--lang` is `rules` by default, and is, therefore, optional. If `-i` is not provided, `stdin` is read. If `-o` is not provided, output goes to `stdout`.

Example:

Generate rules: `fireward -i myrules.ward > firestore.rules`

Generate TypeScript definitions: `fireward -i myrules.ward --lang=typescript > MyTypings.ts`

## Rules Syntax

Fireward tries to keep things simple and easy to learn by mostly using the syntax that already exists in the two languages it generates: Firestore Rules and TypeScript. The basic steps in writing Fireward are: 1. Define a type with TypeScript-like syntax. 2. Assign it to routes written with Firestore Rules syntax. Therefore, the .ward file is essentially the Firestore rules augmented with TypeScript types.

Fireward will wrap the code with the boilerplate: 
```
service cloud.firestore {  
  match /databases/{database}/documents {
    ...
  }
}
```
Currently, it is an error to do so yourself.

### Basic Example

```
type User = {
  name: {first: string, last: string}, // inline child objects
  friends: string[], // a list of strings
  age: int,
  contact?: Phone | Email // a union type
} 
type Phone = {number: int, country: int}
type Email = string

function isUser(userId) { 
  // return keyword optional
  request.auth!=null && request.auth.uid == userId; 
}

match /users/{userId} is User { 
  // read, write, create, update, and delete conditions are allowed
  allow read: if isUser(userId);
  allow create: if isUser(userId);
  allow update: if isUser(userId);
  allow delete: if isUser("admin");
}

```

### Notes on Syntax

#### Types

Firestore rules language' primitive types don't map exactly to JavaScript, so Fireward has to convert them when generating TypeScript definitions. In particular: 
- `int` and `float` map to TypeScript `number`
- `timestamp` maps to `Date|{isEqual: (other: any)=>boolean}`. Snapshots will come as a `Date`, but you can additionally assign a server timestamp object (`firebase.firestore.FieldValue.serverTimestamp`) when writing to the database.
- `bool` in rules maps to TS `boolean`

#### Unions

Union types are supported. Intersections are not. The usage is simple and demonstrated in the basic example above.

#### Lists

Firestore lists are supported and transpile to arrays or tuples in TS. The syntax is `MyType[]` or `MyType[n]`. The second variant will transpile to a MyType tuple up to n in size. For example, if n is 4 (`MyType[4]`), then the result will be a 0,1,2,3 or 4-tuple, basically, an array up to 4 in length. Check the top of the generated TS file for the exported types that represent it.

#### Optional Types and `null`

Unlike in Firebase Realitime Database, optional types differ from `null`s. Optional types are indicated with a `?` before the colon, e.g. `{phone?: string}`. _Warning_: if you are relying on TypeScript, this will allow you to define keys with value `undefined`, which Firestore may reject as an error. Firestore has no equivalent to the JavaScript `undefined`.

#### Punctuation

is important. The example above demonstrates correct usage. Extra or missing marks may cause the file to fail compilation.

#### Route Matching, Conditions and Functions

For the exception of assigning a type to a route, the syntax is identical to the Firestore rule language syntax.

#### Comments

Line comments are supported with `//`.

#### Splitting Across Files

planned but not yet supported

## Contributing

Contributions are welcome!

The project uses the stack tool and puts useful shortcuts, like `make test`, into the makefile.

The project was born from an exercise in monadic programming (based on _Monads for Functional Programming_ by Wadler, and _Thinking Functionally in Haskell_, the chapter on Parsing), so the parser is written from scratch. It seems to be the same in concept as Parsec, but with less functionality.

### Unit testing

Please unit test contributions and make sure all the tests are passing when submitting. The project makes that part easy and fun.

## Roadmap

- Rewrite in actual Parsec to allow for better error messages
- Allow for importing files
- Allow for read/write conditions within types
- Add Windows and Linux release executables pipelines.

## License

MIT github.com/bijoutrouvaille
