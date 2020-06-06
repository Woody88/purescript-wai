module Network.Wai.Internal where 

import Prelude

import Data.List.Lazy as Lzy
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign.Object (Object)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Net.Socket (Socket)
import Node.Stream (Duplex)

type FilePath = String 
type NodeRequest = Foreign

data Request ctx = Request { | BaseContext ctx }

type BaseContext ctx = 
    ( url           :: String 
    , contentLength :: RequestBodyLength
    , method        :: H.Method 
    , headers       :: H.RequestHeaders
    , body          :: Maybe (Lzy.List Buffer)
    | ctx
    ) 

-- newtype Request 
--     = Request { method          :: H.Method 
--               , rawPathInfo     :: String
--               , httpVersion     :: H.HttpVersion
--               , rawQueryString  :: String
--               , requestHeaders  :: H.RequestHeaders
--               , isSecure        :: Boolean
--               , remoteHost      :: String 
--               , pathInfo        :: Array String 
--               , queryString     :: Object String 
--               , body            :: Aff (Maybe Buffer)
--               , bodyLength      :: RequestBodyLength
--               , headerHost      :: String 
--               , headerRange     :: Maybe String 
--               , headerReferer   :: Maybe String 
--               , headerUserAgent :: Maybe String
--               , rawHeader       :: Maybe Buffer
--               , nodeRequest     :: Maybe NodeRequest -- node request internal object
--               }

-- derive instance newtypeRequest :: Newtype Request _ 

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders Duplex
              | ResponseSocket (Socket -> Effect Unit) 
              | ResponseFile   H.Status H.ResponseHeaders String (Maybe FilePart)
              | ResponseRaw (Effect Buffer -> (Effect Buffer -> Effect Unit) -> Effect Unit) Response

data RequestBodyLength = ChunkedBody | KnownLength Int 

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }