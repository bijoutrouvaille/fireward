# Fireward Change Log

## 1.6.0

_Re:_ ternary operators, `readonly` nested objects and `readonly` keyword (instead of `const`), `WardFieldValue`.

_Breaking change:_ Fireward now requires rules version 2, and automatically adds it to the generated rules.
_Breaking change:_ `WardTimestamp` is now defined by using `WardFieldValue`.
_Breaking change:_ TS typings no longer include the Array types for old tuples.
_Breaking change:_ Type `number` is no longer allowed in ward files to help avoid a common type of error.

- `Changelog.md` file.
- Ternary operators in expression parsers are now allowed.
- read only properties now use `readonly` keyword prefixed to the name instead of `const` to the value, just like in TypeScript.
- `read only` (`const`) objects are now checked using the [Firestore Map diff syntax](https://firebase.google.com/docs/reference/rules/rules.MapDiff#changedKeys).
- TS now exports a `WardFieldValue` type to mimic `firestore.FieldValue` more correctly.

#### Internal refactoring

- Simplified the type function calls by using the ternary operators.
