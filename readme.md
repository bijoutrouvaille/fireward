# Fireward

A language and compiler for writing Firestore security rules. The compiler also generates TypeScript interfaces. The main idea is to be able to add idiomatic type validation to routes, as if they had strict type-checking. The language also has some nice features to elegantly express rules for certain situations, which otherwise would be too cumbersome to encode.

[![Mentioned in Awesome awesome-firebase](https://awesome.re/mentioned-badge-flat.svg)](https://github.com/jthegedus/awesome-firebase)

## Discussion

Questions, suggestions, etc.:

https://groups.google.com/forum/#!forum/fireward

## Feature Highlights

- Very fast compilation
- Typed routes that convert to validation rule code.
- `const` types that allow setting but prevent editing of individual fields
- Tuple validation
- Type unions

## Installation

#### Method 1: Binary

Download a [release](https://github.com/bijoutrouvaille/fireward/releases) binary and put it in your `PATH`.

#### Method 2: Compile Yourself

The project is easy to compile for most platforms, requires no knowledge of Haskell, and is completed in two simple steps:

1. Download the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/) for your platform and put it somewhere, e.g. in your `PATH`.
2. `cd` into the project directory and run `stack install`.

Afterwards, everything is automatic. Stack will download and install the compiler and dependencies and put the `fireward` executable into the path given by `stack path --local-bin` command. You may want to add that path to your `PATH` variable. The automatic download and build process will take about 5 minutes, if you have decent bandwidth and processing power.

#### Method 3: Use docker image

Example use to generate firebase rules
```bash
cat definitions.ward | docker run --rm -a stdout -a stdin bijoutrouvaille/fireward > generated.rules
```

Example use to generate typescript rules
```bash
cat definitions.ward | docker run --rm -a stdout -a stdin bijoutrouvaille/fireward -c "--lang=typescript" > generated.ts
```


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

### Simple Example

```
type Name = { first: string, last?: string }
type User = { name: Name | string }

match /users/{id} is User {
  allow read: true;
  allow write: false;
}
```

### Complete Example

```
rules_version = '2' // optional, see https://firebase.google.com/docs/firestore/security/get-started#security_rules_version_2

type User = {
  name: { first: string, last: string }, // inline nested objects
  friends: string[], // a list of strings (string type not validated)
  tags: string[4], // a list of strings, max size 4 (string type IS validated)
  age?: int, // optional type
  verified: bool
  contact: Phone | Email // a union type
  uid: const string // const prevents editing this field later
} 
type Phone = { number: int, country: int }
type Email = string

function isLoggedInUser(userId) { 
  // return keyword optional
  return request.auth!=null && request.auth.uid == userId; 
}

match /users/{userId} is User { 
  // read, write, create, update, list, get and delete conditions are allowed
  allow read, create, update: if isLoggedInUser(userId);
  allow delete: false;
}

```

### Notes on Syntax

#### Types

Firestore rules language' primitive types don't map exactly to JavaScript, so Fireward has to convert them when generating TypeScript definitions. In particular: 
- `int` and `float` map to TypeScript `number`
- `bool` in rules maps to TS `boolean`
- `timestamp` maps to `{seconds: number, nanoseconds: number}|{isEqual: (other: any)=>boolean}`. Snapshots will come as a timestamp (`{seconds: number, nanoseconds: number}`), but when writing to the database, you can assign, in addition to a timestamp, a server timestamp object (`firebase.firestore.FieldValue.serverTimestamp()`).

#### Unions

Union types are supported. Intersections are not. The usage is simple and demonstrated in the basic example above.

#### Lists

Firestore lists are supported and transpile to arrays or tuples in TS. The syntax is `MyType[]` or `MyType[n]`. The second variant will transpile to a MyType tuple up to n in size. For example, if n is 4 (`MyType[4]`), then the result will be a 0,1,2,3 or 4-tuple, basically, an array up to 4 in length. Check the top of the generated TS file for the exported types that represent it.

_Important_: the only way to ensure that the list of, say strings, contains strings is by using the tuple syntax above. Firestore does not have the capability to validate arbitrary size lists.

#### Optional Types and `null`

Unlike in Firebase Realitime Database, optional types differ from `null`s. Optional types are indicated with a `?` before the colon, e.g. `{phone?: string}`. _Warning_: if you are relying on TypeScript, this will allow you to define keys with value `undefined`, which Firestore may reject as an error. Firestore has no equivalent to the JavaScript `undefined`.

#### `const` Types

Fireward allows you to declare primitive types as `const`, as in the example above. A `const` field will permit being written to once, rejecting subsequent writes. By design, the update will also be permitted in situations where the previous value is `null` or optional and absent.

_Warning_: `const` current only works on primitive types. Marking a non-primitive as const will compile without error but do nothing.

#### Route Matching, Conditions and Functions

For the exception of assigning a type to a route, the syntax is identical to the Firestore rule language syntax.

#### Comments

Line comments are supported with `//`.

#### Splitting Across Files

planned but not yet supported

## Contributing

Contributions are welcome!

Please use, test, file issues, star and share Fireward with your Firebase friends. 

### Contributing Code

The project uses the stack tool and puts useful shortcuts, like `make test`, into the makefile.

The project was born from an exercise in monadic programming (based on _Monads for Functional Programming_ by Wadler, and _Thinking Functionally in Haskell_, the chapter on Parsing), so the parser is written from scratch. It seems to be the same in concept as Parsec, but with less functionality.

### Unit testing

Please unit test contributions and make sure all the tests are passing when submitting. The project makes that part easy and fun.

## Status

The project has been recently released, and some bugs are to be expected. The author will try to resolve them soon after they are reported. 

## Roadmap

[x] Add comments
[x] Add error handling to the parser
- Allow for importing files
- Allow for read/write conditions within types
- Add Windows and Linux release executables pipelines.
- Namespace validation functions (e.g. isX for type X)


## License

MIT github.com/bijoutrouvaille
