{ name = "puresweep"
, dependencies =
  [ "arrays"
  , "effect"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "monad-loops"
  , "partial"
  , "prelude"
  , "random"
  , "st"
  , "strings"
  , "tuples"
  , "unordered-collections"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
