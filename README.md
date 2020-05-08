# Wai

A library that provides common protocol (types) for communication between web applications and web servers. 

***This library is a port of the Haskell Wai library. The only difference is that this library pretty much wraps node's core http request classes.***

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding it to your packages.dhall:

```dhall
let additions =
  { wai =
      { dependencies =
        [ "http-types"
        , "node-buffer"
        , "node-http"
        , "node-net"
        , "node-streams"
        , "node-url"
        ]
      , repo =
          "https://github.com/Woody88/purescript-wai.git"
      , version =
          "master"
      }
  }
```
```console
user@user:~$ spago install wai
```

## Usage (**Under Construction**)