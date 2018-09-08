# Changes to migrate from Elm to PureScript


## Syntactic

- Remove `exposing` in module header and add `where`
- Change `:` to `::`
- Change `type` to `data` and `type alias` to `type`


## Semantic

- Use records instead of tuples
- Change `Tuple.first` and `Tuple.second` to record `_.selector`


## Libraries

- Rename `Bool` to `Boolean`
- Explicitly import `Data.List (List)`
- Replace `[]` with `Nil`
- Replace `withDefault` by `fromMaybe`
