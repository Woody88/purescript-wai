module Network.Wai 
    ( module Network.Wai.Types 
    , Application
    , Middleware
    , defaultRequest
    , queryInfo
    , pathInfo
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    ) where 

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Types (Request(..), FilePart, RequestBodyLength(..), Response(..))
import Node.Buffer (Buffer)
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream (Readable)

type Application = Request -> (Response -> Aff Unit) -> Aff Unit
type Middleware = Application -> Application 

defaultRequest :: Request 
defaultRequest = Request
    { url: "" 
    , method: H.GET
    , httpVersion: H.http10  
    , headers: []
    , body: Nothing
    , contentLength: KnownLength 0
    , host: Nothing
    , referer: Nothing 
    , userAgent: Nothing  
    , remoteHost: Nothing
    , range: Nothing
    , isSecure: false
    , reqHandle: Nothing
    }

-- | Creating 'Response' from a string
responseStr :: Status -> ResponseHeaders -> String -> Response
responseStr = ResponseString 

-- | Creating 'Response' from a file.
responseFile :: Status -> ResponseHeaders -> FilePath -> Maybe FilePart -> Response 
responseFile = ResponseFile 

-- | Creating 'Response' from a duplex stream 
responseStream :: Status -> ResponseHeaders -> Readable () -> Response 
responseStream = ResponseStream

-- | Creating 'Response' from a socket
responseSocket :: (Net.Socket -> Maybe Buffer -> Aff Unit) -> Response 
responseSocket = ResponseSocket

-- | will return the request url as an array of string
-- | root will return an empty array
-- |
-- | > req = Wai.defaulRequest { url = "/hello/world" } :: Request
-- | > pathInfo req 
-- | ["hello", "world"] 
-- | 
-- | > req = Wai.defaulRequest { url = "/" } :: Request
-- | > pathInfo req 
-- | [] 
pathInfo :: Request -> Array String 
pathInfo = unwrap >>> _.url >>> split "?" >>> first >>> split "/" >>> nonempty 
  where
    nonempty = Array.filter ((/=) "")
    split = Pattern >>> String.split
    first = Array.head >>> fromMaybe ""

queryInfo :: Request -> Array (Tuple String String)
queryInfo (Request {url: ""}) = []
queryInfo (Request req) = case split "?" req.url of 
  [_, ""] -> []
  [_, qstr] -> parseQueryString qstr
  otherwise -> []
  where 
    parseQueryString = map (splitAt (flip Tuple "") "=") <<< String.split (Pattern "&")
    second = case _ of 
            [_ , qstr] ->  qstr
            _         -> ""
    split = Pattern >>> String.split
    splitAt k p str =
        case String.indexOf (Pattern p) str of
          Just i -> Tuple (String.take i str) (String.drop (i + String.length p) str)
          Nothing -> k str