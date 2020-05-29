module Network.Wai 
    ( Application 
    , Middleware (..)
    , module Network.Wai.Internal
    , defaultRequest
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    , mkNodeRequest
    , unNodeRequest
    ) where 

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object as Object
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Internal (class NodeRequestInternal, FilePart(..), FilePath, NodeRequest(..), Request(..), RequestBodyLength(..), Response(..))
import Node.HTTP as HTTP
import Node.Net.Socket as Net
import Node.Stream as Stream

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
            , httpVersion: H.http10
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

mkNodeRequest :: forall a. NodeRequestInternal a => a -> NodeRequest
mkNodeRequest a = NodeRequest \f -> f a

unNodeRequest :: forall r. (forall a. NodeRequestInternal a => a -> r) -> NodeRequest -> r
unNodeRequest a (NodeRequest f) = f a

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