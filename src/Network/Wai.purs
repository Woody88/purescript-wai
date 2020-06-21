module Network.Wai 
    ( module WaiTypes
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    ) where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.Wai.Types (class WaiRequest, FilePart(..), RequestBodyLength(..), Response(..), body, contentLength, headers, host, httpVersion, method, referer, remoteHost, url, userAgent) as WaiTypes
import Network.Wai.Types (FilePart, Response(..))
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream (Readable)

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
responseSocket :: (Net.Socket -> Effect Unit) -> Response 
responseSocket = ResponseSocket