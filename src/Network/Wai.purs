module Network.Wai 
    ( Application 
    , Middleware (..)
    , module Network.Wai.Internal
    , defaultRequest
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    , nodeHttpRequest
    ) where 

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Foreign.Object as Object
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Internal (FilePart(..), FilePath, Request(..), RequestBodyLength(..), Response(..))
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

type Application = Request -> (Response -> Effect Unit) -> Effect Unit

type Middleware = Application -> Application

-- TO-CONSIDER:
-- Need to expose the node Response in a case where 
-- we need to process something that requires HttpIncomingRequest 
-- from http lib. However, It would be preferrable to have that handle be polymorphic
-- in a situation where we would like to support http2. Maybe existential type can help us here?
defaultRequest :: Request
defaultRequest = 
    Request { method: H.GET
            , rawPathInfo: mempty
            , httpVersion: H.http11
            , rawQueryString: mempty
            , requestHeaders: []
            , isSecure: false
            , remoteHost: ""
            , pathInfo: []
            , queryString: Object.empty
            , body: pure Nothing
            , bodyLength: KnownLength 0
            , headerHost: "Nothing"
            , headerRange: Nothing
            , headerReferer: Nothing
            , headerUserAgent: Nothing
            , rawHeader: Nothing
            , nodeRequest: Nothing
            }

nodeHttpRequest :: Request -> Maybe HTTP.Request 
nodeHttpRequest req 
    | (unwrap req).httpVersion == H.http11 || (unwrap req).httpVersion == H.http10 = unsafeCoerce <$> (unwrap req).nodeRequest
    | otherwise = Nothing 

-- | Creating 'Response' from a string
responseStr :: Status -> ResponseHeaders -> String -> Response
responseStr = ResponseString 

-- | Creating 'Response' from a file.
responseFile :: Status -> ResponseHeaders -> FilePath -> Maybe FilePart -> Response 
responseFile = ResponseFile 

-- | Creating 'Response' from a duplex stream 
responseStream :: Status -> ResponseHeaders -> Stream.Duplex -> Response 
responseStream = ResponseStream

-- | Creating 'Response' from a socket
responseSocket :: (Net.Socket -> Effect Unit) -> Response 
responseSocket = ResponseSocket