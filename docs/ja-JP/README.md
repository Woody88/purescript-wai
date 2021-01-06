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

WebアプリケーションとWebサーバ間の通信のための共通プロトコル(型)を提供するライブラリです。

## インストール

***このライブラリはまだ公開されていません。 
以下の詳細を packages.dhall に追加することで、本パッケージをインストールすることができます。
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

## WAI `アプリケーション`
WAIは`Application`と呼ばれるリクエスト/レスポンスの流れを単純な[CPS](https://ja.wikipedia.org/wiki/継続継続渡しスタイル)関数として表現する。`Application`はリクエストと継続関数を受け取ってから応答を非同期に実行します。継続関数が応答の送信に成功した場合は `ResponseReceived` という値を返す。

```purescript
type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
```

## WAI `ミドルウェア`
二つの `Application` を構成することで `Middleware` を原型することができます。これにより、リクエスト/レスポンスがメインの `Application` に渡す前に変換/検査することが得られます。

```purescript
type Middleware = Application -> Application

-- Stringから 'Response' を作成します。
-- この関数はこのライブラリで提供されています。
responseStr :: Status -> ResponseHeaders -> String -> Response

myCustomMiddleware :: Middleware 
myCustomMiddleware app req send 
    | validatTokenHeader req = app req send 
    | otherwise              = send $ responseStr badRequest400 [] "無効なトークン！"
    where 
        validatTokenHeader :: Request -> Boolean
        validatTokenHeader req = ...
```

ミドルウェアは関数なので、より多くのミドルウェアを互いに組み合わせることができます。

```purescript
type Middleware = Application -> Application

middlewares :: Middleware 
middlewares = myCustomMiddleware1 >>> myCustomMiddleware2

myCustomMiddleware1 :: Middleware 
myCustomMiddleware1 app1 app2 = ...

myCustomMiddleware2 :: Middleware 
myCustomMiddleware app1 app2 = ...
```

## WAIリクエスト `Vault` (TODO)