# Web Application Interface (WAI) 

A library that provides common protocol (types) for communication between web applications and web servers. 

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding the details below to your packages.dhall:
<details>  

```dhall
let additions =
  { wai =
      { dependencies = [ "aff", "effect", "http-types", "node-net", "vault" ]
      , repo =
          "https://github.com/Woody88/purescript-wai.git"
      , version =
          "master"
      }
  , http-types =
      { dependencies = [ "tuples", "unicode", "generics-rep" ]
      , repo =
          "https://github.com/Woody88/purescript-http-types.git"
      , version =
          "master"
      }
    , vault =
        { dependencies =   [ "console", "effect" , "functions" , "maybe" , "prelude" , "psci-support" , "refs" ]
        , repo = "https://github.com/Woody88/purescript-vault.git"
        , version = "master"
        }
  }
```
```console
user@user:~$ spago install wai
```
</details>
</br>

## WAI `Application`
WAI represents request/response flow, referred to as `Application`, as a simple CPS function. An `Application` will accept a request and a continuation function that asynchronously performs the response. If the continuation function successfully sends the response, a `ResponseReceived` value is returned. 

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```

## WAI `Middleware`
By composing two `Application`s we can model a `Middleware`. This gives the ability to transform/inspect the request/response before it is delagated to the main `Application`. 

```purescript
type Middleware = Application -> Application

-- Creating 'Response' from a string
-- This function is provided by this library. 
responseStr :: Status -> ResponseHeaders -> String -> Response

myCustomMiddleware :: Middleware 
myCustomMiddleware app req send 
    | validatTokenHeader req = app req send 
    | otherwise              = send $ responseStr badRequest400 [] "Invalid Token!"
    where 
        validatTokenHeader :: Request -> Boolean
        validatTokenHeader req = ...
```

Because we are modelling the middleware as a function we can just compose more middleware onto one another — this means that we can control the order of the middlewares based on how we compose the functions.

```purescript
type Middleware = Application -> Application

middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app1 app2 = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app1 app2 = ...
```

## WAI Request `Vault` (Under Construction)