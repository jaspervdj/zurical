{ name = "zurihcal"
, dependencies =
  [ "arrays"
  , "datetime-parsing"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "formatters"
  , "integers"
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
