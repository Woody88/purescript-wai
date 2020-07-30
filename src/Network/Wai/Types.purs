module Network.Wai.Types 
    ( Request(..)
    , FilePart(..)
    , RequestBodyLength(..)
    , Response(..)
    )
    where 

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Net.Socket (Socket)
import Node.Stream (Readable)

newtype Request = Request 
    { url           :: String 
    , method        :: H.Method 
    , httpVersion   :: H.HttpVersion  
    , headers       :: H.RequestHeaders
    , body          :: Maybe (Readable ())
    , contentLength :: RequestBodyLength
    , host          :: Maybe String 
    , referer       :: Maybe String 
    , userAgent     :: Maybe String 
    , remoteHost    :: Maybe String
    , range         :: Maybe String
    , isSecure      :: Boolean 
    , reqHandle     :: Maybe Foreign 
    }

derive instance newtypeRequest :: Newtype Request _

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseSocket (Socket -> Maybe Buffer -> Aff Unit)    -- ^ Raw request socket and the first packet of the tunneling stream
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