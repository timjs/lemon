{ name =
    "lemon"
, sources =
    [ "src/**/*.purs" ]
, dependencies =
    [ "console"
    , "effect"
    , "either"
    , "generics-rep"
    , "integers"
    , "lists"
    , "maybe"
    , "numbers"
    , "ordered-collections"
    , "prelude"
    , "psci-support"
    , "string-parsers"
    , "tuples"
    , "unicode"
    , "record"
    , "typelevel-prelude"
    , "enums"
    , "pprint"
    ]
, packages =
    ./packages.dhall
}
