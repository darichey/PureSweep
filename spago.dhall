{ name = "puresweep"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "halogen"
  , "halogen-hooks"
  , "lists"
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
