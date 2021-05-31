{ name = "zurihcal"
, dependencies =
  [ "arrays"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "js-date"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "*.purs" ]
}
