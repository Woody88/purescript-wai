{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-wai"
, dependencies = [ "aff", "effect", "http-types", "node-net", "vault" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
