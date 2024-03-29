# Fireward

A language and compiler for writing Firestore security rules. The compiler also generates TypeScript interfaces. The main idea is to be able to add idiomatic type validation to routes, as if they had strict type-checking. The language also has some nice features to elegantly express rules for certain situations, which otherwise would be too cumbersome to encode.

[![Mentioned in Awesome awesome-firebase](https://awesome.re/mentioned-badge-flat.svg)](https://github.com/jthegedus/awesome-firebase)

## Discussion

Questions, suggestions, etc.:

https://groups.google.com/forum/#!forum/fireward

## Feature Highlights

- Very fast compilation
- Typed routes that convert to validation rule code.
- `readonly` types that allow setting but prevent editing of individual fields
- Tuple validation
- Literal types
- Type unions
- Custom validation expressions in type definitions
- Comments
- A strong unit test suite
- End-to-end tests with emulator

## In the Wild

Fireward was used in these awesome projects:

[StretchMinder](https://github.com/bijoutrouvaille/fireward/issues/29#issuecomment-667219095) is a work break timer for iOS that reminds you to take frequent breaks while providing short guided movement & breathing routines which are designed to address the needs of someone who's sitting all day.

[Oresman Readers Website](https://github.com/bijoutrouvaille/fireward/issues/29#issuecomment-667215177) is a site that houses the famous Oresman Readers, a fine art collection by Donald and Patricia Oresman. It relies on Fireward to structure and secure its data, as well as to provide a shopping cart item reservation system.

[COGS](https://cogs.show/) is an intuitive hardware/software platform for real-world interactive experiences, including escape rooms and immersive theatre.

[Super Magic Link](https://supermagiclink.com) allows Twitch streamers to sync video games, TV shows, movies, and music to their viewers.

Post your project [here](https://github.com/bijoutrouvaille/fireward/issues/29) to be featured in this section.

## Installation

#### Method 0: NPM

`npm install -g fireward` 

Should work for Windows, MacOS and most Linux systems.

#### Method 1: Binary

Download a [release](https://github.com/bijoutrouvaille/fireward/releases) binary and put it in your `PATH`.

#### Method 2: Compile Yourself

The project is easy to compile for most platforms, requires no knowledge of Haskell, and is completed in two simple steps:

1. Download the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/) for your platform and put it somewhere, e.g. in your `PATH`.
2. `cd` into the project directory and run `stack install`.

Afterwards, everything is automatic. Stack will download and install the compiler and dependencies and put the `fireward` executable into the path given by `stack path --local-bin` command. You may want to add that path to your `PATH` variable. The automatic download and build process will take about 5 minutes, if you have decent bandwidth and processing power.

#### Method 3: Use docker image

Install Docker, then use like this:

To generate Firestore rules
```bash
cat definitions.ward | docker run --rm -i -a stdout -a stdin bijoutrouvaille/fireward > generated.rules
```

To generate TypeScript rules
```bash
cat definitions.ward | docker run --rm -i -a stdout -a stdin bijoutrouvaille/fireward -c "--lang=typescript" > generated.ts
```

## Project Status

Fireward has been used in medium-sized projects, in production. It has been built with reliability in mind, which is realized through an extensive unit test suite, an integration test suite, and the uncompromizing type safety of Haskell.

The repository has a committed maintainer, and bugs are fixed quickly. Please be encouraged to use Fireward for your professional work.

## Usage

```
fireward
  -i FILE      --input=FILE                          Input fireward file instead of stdin
  -o FILE      --output=FILE                         Output file instead of stdout
  -s STRING    --string=STRING                       Input string
  -l language  --lang=language, --language=language  Output language. One of: rules, typescript (or ts).
  -V           --version                             Print version
  -h           --help                                Show help
```

`--lang` is `rules` by default, and is, therefore, optional. If `-i` is not provided, `stdin` is read. If `-o` is not provided, output goes to `stdout`.

Example:

Generate rules: `fireward -i myrules.ward > firestore.rules`

Generate TypeScript definitions: `fireward -i myrules.ward --lang=typescript > MyTypings.ts`

## Rules Syntax

Fireward tries to keep things simple and easy to learn by mostly using the syntax that already exists in the two languages it generates: Firestore Rules and TypeScript. The basic steps in writing Fireward are: 1. Define a type with TypeScript-like syntax. 2. Assign it to routes written with Firestore Rules syntax. Therefore, the .ward file is essentially Firestore rules augmented with TypeScript types.

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
  allow read: true; // will allow all reads
  allow create, update: request.auth!=null; // will only allow creates and updates if logged in AND input is of type User
  allow delete: false; // will never allow deletes
}
```

### Complete Example

```
rules_version = '2' // unnecessary, see notes on Version 2 below

type User = {

  name: { first: string, last: string }, // inline nested objects
  age?: int, // optional type
  verified: bool

  contact: Phone | Email // a union type

  // readonly prevents editing this field later (the old `const` syntax will continue to work also)
  // this works for both literals and nested objects
  readonly uid: string 

  smorgasBoard: "hi" | "bye" | true | 123 // literal types, same as in TS

  friends: string[], // a list of strings (string type not validated)
  flags: [string, 'new' | 'verified', string | int, string?], // a 4-tuple, max size 4, last item optional (type IS validated)

  permissions: map // corresponds to `map` type in the rules and `Record<string, unknown>` in TS

  irrelevantType: any // translates to the `any` type in typescript and is not type checked in the rules

  location: latlng // native Firestore geolocation type

  "ハロー・ワールド": string // for valid property names see https://firebase.google.com/docs/firestore/quotas#collections_documents_and_fields

  // Custom type validation expressions can appear at the end of any type
  allow update: if data.age > prev.age // data refers to this type's incoming data, prev refers to previously stored data. 
  allow write: if request.time > 123 // shorthand for create, update, delete
  allow create, update: if data.verified == true // allows to group multiple methods into a single expression
} 

type Phone = { number: int, country: int }
type Email = string

function isLoggedInUser(userId) { 
  let a = request.auth;
  // return keyword optional
  return a!=null && a.uid == userId; 
}

match /users/{userId} is User { 
  // read, write, create, update, list, get and delete conditions are allowed
  allow read, create, update: if isLoggedInUser(userId); // anti-pattern: combining reads and writes like this is possible but dangerous. 
  allow delete: false;
}

```

### Notes on Syntax

#### Rules version 2

Firestore rules can operate under the old or new syntax: see https://firebase.google.com/docs/firestore/security/get-started#security_rules_version_2.

Fireward only supports version 2, and will automatically include the setting in generated rules.

#### Type Mapping

Firestore rules language' primitive types don't map exactly to JavaScript, so Fireward has to convert them when generating TypeScript definitions. In particular: 

- `int` and `float` map to TypeScript `number`
- `bool` in rules maps to TS `boolean`
- `latlng` in rules maps to TS `WardGeoPoint` which tries to mimic the Web native type https://firebase.google.com/docs/reference/js/firebase.firestore.GeoPoint.
- `timestamp` maps to `null|WardTimestamp|Date|WardFieldValue`. Snapshots will come in as a `WardTimestamp`, but when writing to the database, you can assign, in addition to a `WardTimestamp` object, a server `timestamp` object (`firebase.firestore.FieldValue.serverTimestamp()`) or a javascript `Date` object. `WardTimestamp` is defined in the generated typescript file, and it is intended to follow the officially defined interface: https://firebase.google.com/docs/reference/node/firebase.firestore.Timestamp.html.

#### Unions

Union types are supported. Intersections are not (yet). The usage is simple and demonstrated in the basic example above.

#### Lists

Firestore lists are supported and transpile to arrays in TS. The syntax is `MyType[]`. The type is not validated in Firestore.

_Note:_ the syntax `MyType[n]` will continue to work for some time, but it is being phased out. Please use the tuple syntax.

#### Tuples

The tuple syntax is very similar to TS. 

- Any supported type can be an element of the tuple.
- To make an element optional, suffix it with a question mark.

Example: `[string, 'hello', 123, int?, (string | MyType)?]`

The above requires the first three elements present, but the last two are optional.

#### Optional Types and `null`

Unlike in Firebase Realitime Database, optional types differ from `null`s. Optional types are indicated with a `?` before the colon in objects, e.g. `{phone?: string}`, or after the type name in tuples. _Warning_: if you are relying on TypeScript, an optional type will allow you to define keys with value `undefined`, which Firestore may reject as an error. Firestore has no equivalent to the JavaScript `undefined`.

#### `readonly` Types

Fireward allows you to declare properties as `readonly`, as in the example above. A `readonly` property will permit being written to once, rejecting subsequent writes. By design, the update will also be permitted in situations where the previous value is `null` or optional _and_ absent.

_Note_: `readonly` used to only work on primitive types. Now it works for nested objects as well, thanks to the firestore [MapDiff feature](https://firebase.google.com/docs/reference/rules/rules.MapDiff).

#### Type validation Expressions

Each type definition, including inline and nested objects, can have custom validations _at the end._

- The syntax is the same as in route conditions: `allow method1, method2: if <condition expr>`. `if` is optional.
- Allowed methods are `create`, `update`, `delete`, and `write`. 
- `write` is simply a shorthand for all the other ones. 
- Automatic variables `data` and `prev` refer to incoming data and previously stored data _for this type_ respectively.
- `data` will be null on deletes, and `prev` will be null on creates.
- Custom validation expressions are combined with other checks (route conditions and type checks) using the logical AND operator. If either one fails, all fail.
- Multiple expressions can coexist for the same request method and will combine using the logical AND, as above.

_Note:_ if a validation is absent for a method and type, it will pass. For example, if you have create and delete validations, but not an update one, updates will pass validation, unless other conditions (e.g. route-level conditions or variable type validation) prevent it.

See the complete example above.

#### Literal Types

Just like the Typescript language, Fireward supports literal types that can be mixed and matched using unions. The supported literal values are `true`, `false`, numbers and strings. For example, the following Fireward line will map to the exact same Typescript code `type SmorgasBoard = "hello" | 'bye' | 123 | 2.5 | false` and can be used to validate routes in Firestore.

#### Route Matching, Conditions and Functions

For the exception of assigning a type to a route, the syntax is identical to the Firestore rule language syntax.

#### Comments

Line comments are supported with `//`.


#### Input/Output Generics 

_Note:_ The usage of this feature is optional, though very convenient.

Types that go _into_ the database are different from those that come _out_ of it. For example, a `float` value obtained from the database will become a Typescript `number`, but when you send it to the Firestore client to be saved in the database, its value can be also `firestore.FieldValue.increment()`, which returns a `FieldValue` class, aka Sentinel value. Timestamps are a more complex example of this difference. When Typescript typings are generated, this creates a problem: which Firestore types do we reflect in the Typescript definitions? Fireward deals with this issue by providing types as an optional generic parameter. For example, the following Fireward definition

```
type Person = {
  name: string
  age: int
}
```

generates this Typescript

```
type Person<Types extends FirewardTypes = FirewardTypes> = {
  name: string
  age: Types['number']
}
```

`FirewardTypes` is provided with the generated TS like so `export type FirewardTypes = FirewardInput | FirewardOutput`. `FirewardInput` and `FirewardOutput` are also provided:

```
export type FirewardOutput = /** what you get from DB */ { timestamp: WardTimestamp|null; number: number; };
export type FirewardInput = /** what you send to DB */ { timestamp: WardTimestamp|Date|WardFieldValue; number: number|WardFieldValue; };
```

You can now see the flexibility this provides. If you want to limit your `Person` type only to values that may come from the database, simply pass the corresponding type parameter `type PersonInput = Person<FirewardInput>`. You can also define your own IO types, which will work as long they are assignable to the type that defines _all_ the possible ones--`FirewardTypes`.


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

### Integration testing

Is done from the `integration` directory. 

- You must have firebase-tools installed globally and firestore emulator set up by running `firebase setup:emulators:firestore`. 
- Run the tests with `npm test` command. 
- You do not need to launch the emulator. The test runner will automatically do that for you.

The tests are writted with mocha and typescript. Put fixture ward files in the `integration/wards` folder.



## Roadmap

- [x] Line comments
- [x] Error handling to the parser
- [x] Windows and Linux release executables pipelines
- [x] End-to-end tests with the Firestore emulator
- [x] Namespace validation functions (e.g. `isX` for `type X` should have a better name space). Done: now: `is____X`.
- [x] String, numeric and boolean literal types
- [x] Custom validation expressions within type definitions
- [x] Actual tuple types, e.g. [int, string, MyType]
- [x] Expression variables
- [x] Refactor with ternary types
- [x] Implement readonly/const objects using the new Map Diffs.
- [x] Input/Output types for typescript (see io-generics branch)
- [ ] Generic types
- [ ] Importing files
- [ ] Type intersections


## License

MIT github.com/bijoutrouvaille
