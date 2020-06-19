module Network.Wai.Internal where 

import Prelude

import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Foreign.Object as Object
import Network.HTTP.Types (http09, http10, http11)
import Network.HTTP.Types as H
import Network.HTTP.Types as Method
import Node.Buffer (Buffer)
import Node.HTTP as HTTP
import Node.Net.Socket (Socket)
import Node.Stream (Duplex, Readable)
import Unsafe.Coerce (unsafeCoerce)

type FilePath = String 

class WaiRequest hdl where 
    url           :: hdl -> String 
    method        :: hdl -> H.Method 
    httpVersion   :: hdl -> H.HttpVersion  
    headers       :: hdl -> H.RequestHeaders
    bodyStream    :: hdl -> Readable ()
    contentLength :: hdl -> RequestBodyLength

instance waiRequestHTTP :: WaiRequest HTTP.Request where 
    url           = _.url <<< unsafeCoerce
    method        = Method.fromString <<< _.method <<< unsafeCoerce
    httpVersion   = parseHttpVersion <<< _.httpVersion <<< unsafeCoerce 
        where 
            parseHttpVersion = case _ of 
                "0.9"     -> http09
                "1.1"     -> http11
                otherwise -> http10

    headers       = Object.toUnfoldable <<< _.headers <<< unsafeCoerce 
    bodyStream    = unsafeCoerce
    contentLength = parseContentLength <<< Map.lookup "content-length" <<< Map.fromFoldable <<< hdrs
        where 
            hdrs :: HTTP.Request -> Array (Tuple String String) 
            hdrs = Object.toUnfoldable <<< _.headers <<< unsafeCoerce 
            parseContentLength = 
                maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) 

    -- = Request { method          :: H.Method 
    --           , rawPathInfo     :: String
    --           , httpVersion     :: H.HttpVersion
    --           , rawQueryString  :: String
    --           , requestHeaders  :: H.RequestHeaders
    --           , isSecure        :: Boolean
    --           , remoteHost      :: String 
    --           , pathInfo        :: Array String 
    --           , queryString     :: Object String 
    --           , body            :: Aff (Maybe Buffer)
    --           , bodyLength      :: RequestBodyLength
    --           , headerHost      :: String 
    --           , headerRange     :: Maybe String 
    --           , headerReferer   :: Maybe String 
    --           , headerUserAgent :: Maybe String
    --           , rawHeader       :: Maybe Buffer
    --           , nodeRequest     :: Maybe NodeRequest -- node request internal object
    --           }

-- derive instance newtypeRequest :: Newtype (Request hdl) _ 

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