<div align="center">
  <h1>
    <code>WAI</code>: Web Application Interface
  </h1>
</div>

<p align="center">
  <a href="https://github.com/Woody88/purescript-wai/blob/master/README.md"
    ><img
      height="30"
      src="https://raw.githubusercontent.com/Woody88/purescript-wai/master/docs/media/flag-ca.png"
      alt="English" /></a>
  &nbsp;
  <a
    href="https://github.com/Woody88/purescript-wai/blob/master/docs/ja-JP/README.md"
    ><img
      height="30"
      src="https://raw.githubusercontent.com/Woody88/purescript-wai/master/docs/media/flag-ja.png"
      alt="日本語" /></a>
  &nbsp;
  <a
    href="https://github.com/Woody88/purescript-wai/blob/master/docs/fr-FR/README.md"
    ><img
      height="30"
      src="https://raw.githubusercontent.com/Woody88/purescript-wai/master/docs/media/flag-fr.png"
      alt="Français" /></a>
  &nbsp;
</p>

This library provides a common interface for communication between web applications and web servers. 

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding the details below to your packages.dhall:

<details>
  <summary><strong>Using Spago</strong></summary>

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

## `Application`

WAI models applications using a request-response flow.

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```

An application is a function that receives a request along with a continuation function for sending the response.

## `Middleware`

Middleware is an application transformer. That is, a function from application to application:

```purescript
type Middleware = Application -> Application
```

Because these are simply functions, they can be composed in any order.

```purescript
middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app req send = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app req send = ...
```

This enables us to write declarative, composable middleware:

```purescript
requireAuthToken :: Middleware 
requireAuthToken app req send 
    | hasAuthToken req = app req send 
    | otherwise        = send $ responseStr unauthorized401 [] "Missing Token!"
    where 
        hasAuthToken :: Request -> Boolean
        hasAuthToken req = ...

-- Creates a `Response` from a string. This helper function is provided by WAI. 
responseStr :: Status -> ResponseHeaders -> String -> Response
```

## Request `Vault` (Under Construction)
