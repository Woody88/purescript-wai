module Network.Wai.Internal where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Socket (Socket)
import Node.Stream (Duplex)

type FilePath = String 

newtype Request 
    = Request { method          :: H.Method 
              , rawPathInfo     :: String
              , httpVersion     :: H.HttpVersion
              , rawQueryString  :: String
              , requestHeaders  :: H.RequestHeaders
              , isSecure        :: Boolean
              , remoteHost      :: String 
              , pathInfo        :: Array String 
              , queryString     :: Object String 
              , body            :: Aff (Maybe Buffer)
              , bodyLength      :: RequestBodyLength
              , headerHost      :: String 
              , headerRange     :: Maybe String 
              , headerReferer   :: Maybe String 
              , headerUserAgent :: Maybe String
              , rawHeader       :: Maybe Buffer
              , nodeRequest     :: Maybe HTTP.Request
              }

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders Duplex
              | ResponseSocket (Socket -> Effect Unit) 
              | ResponseFile   H.Status H.ResponseHeaders String

data RequestBodyLength = ChunkedBody | KnownLength Int 
