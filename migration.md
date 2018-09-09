# Migration from Elm to PureScript


## Language

- Remove `exposing` in module header and add `where`
- Change `:` to `::`
- Change `type` to `data` and `type alias` to `type`
- Option to use equations instead of `case-of`
- Option to use guards instead of `if-then-else`
- Option to use `where` instead of `let`


## Libraries

- Use `Tuple a b` instead of `( a, b )`
  or use records
- Change `Tuple.first` and `Tuple.second` to `fst` and `snd`
  or to record `_.selector`
- Rename `Bool` to `Boolean`
- Rename `Dict` to `Map`
- Explicitly import `Data.List (List)`
- Replace `[]` with `Nil`
- Replace `withDefault` by `fromMaybe`


## Remarks

### The good

- Holes `?hole` are amazing!
- Finally use type classes!
- Finally more generic abstractions `func :: Constraint => Type`!
- Deriving `derive instance name :: Constraint => Class` is amazing!


### The bad

- New module imports hurt sometimes:
  - Modules cannot be found
  - Restarting `purs-ide` doesn't help
  - => need to build newly installed modules first!
- `import X.Y.Z (a, b, c) as Z` seems not to bring `Z` into scope like `import X.Y.Z as Z`
  - => Semantic difference: import specified names under Z and import everything under Z
  - => Read `import X.Y.Z as Z` as `import X.Y.Z (..) as Z`
- https://stackoverflow.com/questions/38488456/foldable-for-tree-type-value-of-instance-is-undefined-here-so-this-reference


### The Ugly
