module Network.Wai.Internal where 

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Tuple (Tuple)
import Effect (Effect)
import Foreign.Object as Object
import Network.HTTP.Types (http09, http10, http11)
import Network.HTTP.Types as H
import Network.HTTP.Types as Method
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Socket (Socket)
import Node.Net.Socket as Socket
import Node.Stream (Readable)
import Unsafe.Coerce (unsafeCoerce)

class WaiRequest hdl where 
    url           :: hdl -> String 
    method        :: hdl -> H.Method 
    httpVersion   :: hdl -> H.HttpVersion  
    headers       :: hdl -> H.RequestHeaders
    body          :: hdl -> Readable ()
    contentLength :: hdl -> RequestBodyLength
    host          :: hdl -> String 
    referer       :: hdl -> Maybe String 
    userAgent     :: hdl -> String 
    remoteHost    :: hdl -> Effect (Maybe String)

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseSocket (Socket -> Effect Unit) 
              | ResponseFile H.Status H.ResponseHeaders String (Maybe FilePart)
              | ResponseRaw (Effect Buffer -> (Effect Buffer -> Effect Unit) -> Effect Unit) Response

data RequestBodyLength = ChunkedBody | KnownLength Int 

instance showRequestBodyLength :: Show RequestBodyLength where 
    show = case _ of 
        ChunkedBody -> "<ChunkedBody>"
        KnownLength l -> "KnownLength " <> show l

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }