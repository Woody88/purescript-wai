let conf = ./spago.dhall

let dependencies = [ "console" ]

let sources = [ "test/**/*.purs" ]

in conf //
  { dependencies = conf.dependencies # dependencies
  , sources = conf.sources # sources
  }