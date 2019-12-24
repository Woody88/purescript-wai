module Network.Wai.Internal where 

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Network.HTTP.Types as H 
import Node.Buffer (Buffer)
import Node.Stream (Readable)
import Network.HTTP.Types (Host, Path, QueryPairs) as URI
import Network.HTTP.Types (Key, Value)

type FilePath = String 

newtype Request 
    = Request { requestMethod     :: H.Method 
              , rawPathInfo       :: String
              , httpVersion       :: H.HttpVersion
              , rawQueryString    :: String
              , requestHeaders    :: H.RequestHeaders
              , isSecure          :: Boolean
              , remoteHost        :: URI.Host 
              , pathInfo          :: URI.Path
              , queryString       :: URI.QueryPairs Key Value
              , body              :: Aff (Maybe Buffer)
              , bodyLength        :: RequestBodyLength
              }

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseFile   H.Status H.ResponseHeaders String

data RequestBodyLength = ChunkedBody | KnownLength Int 
