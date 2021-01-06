<div align="center">
<h1>
WAI</br>
Web Application Interface 
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

Une bibliothèque qui fournit des protocoles (types) communs pour la communication entre les applications web et les serveurs web. 

## Installation

***Cette bibliothèque n'est pas encore publiée sur pursuit.***  
Vous pouvez installer ce paquet en ajoutant les détails ci-dessous à votre packages.dhall :
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

## WAI `Application`.
WAI représente le flux de requête/réponse, appelé `Application`, en tant que simple fonction [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style). Une `Application` accepte une requête et une fonction de continuation qui effectue la réponse de manière asynchrone. Si la fonction de continuation envoie avec succès la réponse, une valeur "ResponseReceived" est retourné. 

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```

## WAI `Middleware` (logiciel intermédiaire)
En composant deux `Applications`, nous pouvons modéliser un `Middleware`. Cela permet de transformer/inspecter la requête/réponse avant qu'elle ne soit délocalisée vers l'`Application` principale. 

```purescript
type Middleware = Application -> Application

-- Crée une "Response" à partir d'un String
-- Cette fonction fait partie de cette bibliothèque. 
responseStr :: Status -> ResponseHeaders -> String -> Response

myCustomMiddleware :: Middleware 
myCustomMiddleware app req send 
    | validatTokenHeader req = app req send 
    | otherwise              = send $ responseStr badRequest400 [] "Jeton invalide!"
    where 
        validatTokenHeader :: Request -> Boolean
        validatTokenHeader req = ...
```

Comme nous modélisons le middleware en tant que fonction, nous pouvons simplement composer plusieurs middleware les uns sur les autres - cela signifie que nous pouvons contrôler l'ordre des middlewares en fonction de la façon dont nous composons les fonctions.

```purescript
type Middleware = Application -> Application

middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app1 app2 = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app1 app2 = ...
```

## WAI Request `Vault` (En Construction)