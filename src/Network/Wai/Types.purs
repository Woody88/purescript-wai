module Network.Wai.Types 
    (class WaiRequest
    , FilePart(..)
    , RequestBodyLength(..)
    , Response(..)
    , body
    , contentLength
    , headers
    , host
    , httpVersion
    , method
    , referer
    , remoteHost
    , url
    , userAgent
    )
    where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
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
              | ResponseSocket (Socket -> Maybe Buffer -> Aff Unit) 
              | ResponseFile H.Status H.ResponseHeaders String (Maybe FilePart)
              | ResponseRaw (Aff Buffer -> (Aff Buffer -> Aff Unit) -> Aff Unit) Response

data RequestBodyLength = ChunkedBody | KnownLength Int 

instance showRequestBodyLength :: Show RequestBodyLength where 
    show = case _ of 
        ChunkedBody -> "<ChunkedBody>"
        KnownLength l -> "KnownLength " <> show l

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }