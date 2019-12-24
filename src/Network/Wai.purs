module Network.Wai where 

import Prelude (Unit, mempty, (<$>))

import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Class (liftEffect)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Internal (FilePath, Request(..), RequestBodyLength(..), Response(..))
import Node.Buffer as Buffer
import Node.Stream as Stream
import URI.Extra.QueryPairs (QueryPairs(..))
import URI.Host (Host(..))
import URI.Path (Path(..))
import URI.Host.IPv4Address (unsafeFromInts) as Host

type Application = Request -> (Response -> Effect Unit) -> Effect Unit

type Middleware = Application -> Application

defaultRequest :: Request
defaultRequest = 
    Request { requestMethod: H.GET
            , rawPathInfo: mempty
            , httpVersion: H.http10
            , rawQueryString: mempty
            , requestHeaders: []
            , isSecure: false
            , remoteHost: IPv4Address (Host.unsafeFromInts 0 0 0 0)
            , pathInfo: Path []
            , queryString: QueryPairs []
            , body:  liftEffect (Just <$> Buffer.create 0)
            , bodyLength: KnownLength 0
            }

-- | Creating 'Response' from 'L.ByteString'. This is a wrapper for
--   'responseBuilder'. 
responseStr :: Status -> ResponseHeaders -> String -> Response
responseStr = ResponseString 

-- | Creating 'Response' from a file.
responseFile :: Status -> ResponseHeaders -> FilePath -> Response 
responseFile = ResponseFile 

-- | Creating 'Response' from a readable stream 
responseStream :: Status -> ResponseHeaders -> Stream.Readable () -> Response 
responseStream = ResponseStream
