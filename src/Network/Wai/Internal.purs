module Network.Wai.Internal 
    ( Request(..)
    , FilePart(..)
    , RequestBodyLength(..)
    , Response(..)
    , ResponseReceived
    )
    where 

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Net.Socket (Socket)
import Node.Stream (Readable)
import Data.Vault (Vault)

newtype Request = Request 
    { url           :: String 
    , method        :: H.StdMethod
    , pathInfo      :: Array String 
    , queryString   :: H.Query    
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
    -- | A location for arbitrary data to be shared by applications and middleware.
    , vault         :: Vault 
    }

derive instance newtypeRequest :: Newtype Request _

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseSocket (Socket -> Maybe Buffer -> Aff Unit)    -- ^ Raw request socket, special uses case need when upgrade request (i.e: websocket).
              | ResponseFile H.Status H.ResponseHeaders String (Maybe FilePart)

data RequestBodyLength = ChunkedBody | KnownLength Int 

instance showRequestBodyLength :: Show RequestBodyLength where 
    show = case _ of 
        ChunkedBody -> "<ChunkedBody>"
        KnownLength l -> "KnownLength " <> show l

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }

-- | A datatype to indicate that the WAI handler has received the
-- response.
data ResponseReceived = ResponseReceived