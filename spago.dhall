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
  , "tuples"
  , "unordered-collections"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
