{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-wai"
, dependencies =
  [ "effect"
  , "aff"
  , "http-types"
  , "node-net"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
