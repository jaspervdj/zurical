{ name = "zurihcal"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "js-date"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
