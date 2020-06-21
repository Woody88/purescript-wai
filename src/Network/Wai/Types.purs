module Network.Wai.Types where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Net.Socket (Socket)
import Node.Stream (Readable)

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