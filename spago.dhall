{ name = "puresweep"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "monad-loops"
  , "partial"
  , "prelude"
  , "random"
  , "st"
  , "transformers"
  , "tuples"
  , "unordered-collections"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
