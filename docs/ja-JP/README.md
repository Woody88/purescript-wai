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

WebアプリケーションとWebサーバ間の通信のための共通プロトコルを提供するライブラリです。

## インストール

***このライブラリはまだ公開されていません。 
以下の詳細を packages.dhall に追加することで、本パッケージをインストールすることができます。
<details>  
  <summary><strong>Spagoで</strong></summary>

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

## `アプリケーション`
WAIはリクエストレスポンスフローを`Application`型で原型しています。

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```
`Application`とは、レスポンスを送信するための継続関数と、リクエストを受け取る関数です。

## `ミドルウェア`

ミドルウェアはアプリケーション変換子、つまりアプリケーションからアプリケーションへの関数です。

```purescript
type Middleware = Application -> Application
```

これらは単なる関数なので、好きな順番で合成することができます。

```purescript
middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app1 app2 = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app1 app2 = ...
```

これにより、宣言的で合成可能なミドルウェアを記述することができます:

```purescript
requireAuthToken :: Middleware 
requireAuthToken app req send 
    | hasAuthToken req = app req send 
    | otherwise        = send $ responseStr unauthorized401 [] "無効なトークン！"
    where 
        hasAuthToken :: Request -> Boolean
        hasAuthToken req = ...

-- Stringから 'Response' を作成します。
-- この関数はこのライブラリで提供されています。
responseStr :: Status -> ResponseHeaders -> String -> Response
```

## WAIリクエスト `Vault` (TODO)