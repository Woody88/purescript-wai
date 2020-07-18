module Network.Wai 
    ( module WaiTypes
    , defaultRequest
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    ) where 

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.HTTP.Types as H
import Network.Wai.Types (Request(..), FilePart(..), RequestBodyLength(..), Response(..)) as WaiTypes
import Network.Wai.Types (Request(..), FilePart, RequestBodyLength(..), Response(..))
import Node.Buffer (Buffer)
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream (Readable)

defaultRequest :: Request
defaultRequest = Request
    { url: mempty 
    , method: H.GET
    , httpVersion: H.http11 
    , headers: mempty 
    , body: Nothing
    , contentLength: KnownLength 0
    , host: mempty 
    , referer: Nothing 
    , userAgent: mempty  
    , remoteHost: pure Nothing 
    , isSecure: false 
    , handle: Nothing 
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