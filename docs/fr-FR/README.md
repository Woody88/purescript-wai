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

Une bibliothèque qui fournit des protocoles communs pour la communication entre les applications web et les serveurs web. 

## Installation

***Cette bibliothèque n'est pas encore publiée sur pursuit.***  
Vous pouvez installer ce paquet en ajoutant les détails ci-dessous à votre packages.dhall :

<details>  
  <summary><strong>Utilisant Spago</strong></summary>

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

## `Application`.
WAI représente WAI modélise les applications en utilisant un flux de requête-réponse. 

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```

Une application est une fonction qui reçoit une requête avec une fonction de continuation pour l'envoi de la réponse.

## `Middleware`

Le middleware est un transformateur d'applications. C'est-à-dire une fonction d'application en application :

```purescript
type Middleware = Application -> Application
```

Comme il s'agit simplement de fonctions, elles peuvent être composées dans n'importe quel ordre.

```purescript
middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app req send = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app req send = ...
```

Cela nous permet d'écrire un middleware déclaratif et composable :

```purescript
requireAuthToken :: Middleware 
requireAuthToken app req send 
    | hasAuthToken req = app req send 
    | otherwise        = send $ responseStr unauthorized401 [] "Missing Token!"
    where 
        hasAuthToken :: Request -> Boolean
        hasAuthToken req = ...

-- Crée une "Response" à partir d'un String
-- Cette fonction fait partie de cette bibliothèque. 
responseStr :: Status -> ResponseHeaders -> String -> Response
```

## Request `Vault` (En Construction)