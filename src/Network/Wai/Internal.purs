module Network.Wai.Internal where 

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
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

instance waiRequestHTTP :: WaiRequest HTTP.Request where 
    url           = _.url <<< unsafeCoerce
    method        = Method.fromString <<< _.method <<< unsafeCoerce
    httpVersion   = parseHttpVersion <<< _.httpVersion <<< unsafeCoerce 
        where 
            parseHttpVersion = case _ of 
                "0.9"     -> http09
                "1.1"     -> http11
                otherwise -> http10

    headers       = httpHeaders
    body    = unsafeCoerce
    host    = fromMaybe mempty <<< Map.lookup "host" <<< Map.fromFoldable <<< httpHeaders
    referer = Map.lookup "referer" <<< Map.fromFoldable <<< httpHeaders
    userAgent = fromMaybe mempty <<< Map.lookup "user-agent" <<< Map.fromFoldable <<< httpHeaders
    remoteHost = remoteHost' <<< _.socket <<< unsafeCoerce
        where 
            remoteHost' socket = do 
                let mkHost mhost mport = (\host port -> String.joinWith ":" [host, show port]) <$> mhost <*> mport 
                mkHost <$> Socket.remoteAddress socket  <*> Socket.remotePort socket 

    contentLength = parseContentLength <<< Map.lookup "content-length" <<< Map.fromFoldable <<< httpHeaders
        where 
            parseContentLength = 
                maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) 

httpHeaders :: HTTP.Request -> Array (Tuple String String) 
httpHeaders = Object.toUnfoldable <<< _.headers <<< unsafeCoerce

data Response = ResponseString H.Status H.ResponseHeaders String
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseSocket (Socket -> Effect Unit) 
              | ResponseFile H.Status H.ResponseHeaders String (Maybe FilePart)
              | ResponseRaw (Effect Buffer -> (Effect Buffer -> Effect Unit) -> Effect Unit) Response

data RequestBodyLength = ChunkedBody | KnownLength Int 

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }