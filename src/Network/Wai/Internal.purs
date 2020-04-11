module Network.Wai.Internal where 

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Network.HTTP.Types (HostPortPair, Path, QueryPairs) as URI
import Network.HTTP.Types (Key, Value, Host, Port)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Stream (Duplex)

type FilePath = String 

newtype Request 
    = Request { method          :: H.Method 
              , rawPathInfo     :: String
              , httpVersion     :: H.HttpVersion
              , rawQueryString  :: String
              , requestHeaders  :: H.RequestHeaders
              , isSecure        :: Boolean
              , remoteHost      :: URI.HostPortPair Host Port 
              , pathInfo        :: URI.Path
              , queryString     :: URI.QueryPairs Key Value
              , body            :: Aff (Maybe Buffer)
              , bodyLength      :: RequestBodyLength
              , headerHost      :: URI.HostPortPair Host Port 
              , headerRange     :: Maybe String 
              , headerReferer   :: Maybe String 
              , headerUserAgent :: Maybe String 
              }

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders Duplex
              | ResponseFile   H.Status H.ResponseHeaders String

data RequestBodyLength = ChunkedBody | KnownLength Int 
