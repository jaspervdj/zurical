{ name = "zurihcal"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "js-date"
  , "lists"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "psci-support"
  , "unordered-collections"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
