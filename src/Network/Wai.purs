module Network.Wai 
    ( module WaiTypes
    , Application
    , Middleware
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    ) where 

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.Wai.Types (class WaiRequest, FilePart(..), RequestBodyLength(..), Response(..), body, isSecure, contentLength, headers, host, httpVersion, method, referer, remoteHost, url, userAgent) as WaiTypes
import Network.Wai.Types (class WaiRequest, FilePart, Response(..))
import Node.Buffer (Buffer)
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream (Readable)

type Application = forall req. WaiRequest req => req -> (Response -> Aff Unit) -> Aff Unit
type Middleware = Application -> Application 

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