module Network.Wai 
    ( module Network.Wai.Internal 
    , Application
    , Middleware
    , defaultRequest
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    , modifyResponse
    , ifRequest
    ) where 

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vault as Vault
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Internal (FilePart, Request(..), ResponseReceived, RequestBodyLength(..), Response(..))
import Node.Buffer (Buffer)
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream (Readable)

type Application = Request -> (Response -> Aff ResponseReceived) -> Aff ResponseReceived
type Middleware = Application -> Application 

defaultRequest :: Request 
defaultRequest = Request
    { url: "" 
    , method: H.GET
    , httpVersion: H.http11
    , headers: []
    , pathInfo: []
    , queryString: []
    , body: Nothing
    , contentLength: KnownLength 0
    , host: Nothing
    , referer: Nothing 
    , userAgent: Nothing  
    , remoteHost: Nothing
    , range: Nothing
    , isSecure: false
    , vault: Vault.empty
    }

-- | Creating 'Response' from a string
responseStr :: Status -> ResponseHeaders -> String -> Response
responseStr = ResponseString 

-- | Creating 'Response' from a file.
responseFile :: Status -> ResponseHeaders -> FilePath -> Maybe FilePart -> Response 
responseFile = ResponseFile 

-- | Creating 'Response' from a duplex stream 
responseStream :: Status -> ResponseHeaders -> Readable () -> Response 
responseStream = ResponseStream

-- | Creating 'Response' from a socket
responseSocket :: (Net.Socket -> Maybe Buffer -> Aff Unit) -> Response 
responseSocket = ResponseSocket

-- | Apply/execute a function that modifies a response as a 'Middleware'
modifyResponse :: (Response -> Response) -> Middleware
modifyResponse f app req respond = app req $ respond <<< f

-- | Conditionally apply/execute a 'Middleware'
ifRequest :: (Request -> Boolean) -> Middleware -> Middleware
ifRequest rpred middleware app req 
    | rpred req = middleware app req
    | otherwise = app req