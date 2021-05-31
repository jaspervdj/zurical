{ name = "zurihcal"
, dependencies =
  [ "arrays"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "js-date"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "*.purs" ]
}
