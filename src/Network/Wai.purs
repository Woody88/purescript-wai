module Network.Wai 
    ( Application 
    , Middleware (..)
    , module Network.Wai.Internal
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    ) where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.Wai.Internal (class WaiRequest, FilePart(..), RequestBodyLength(..), Response(..))
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
responseSocket :: (Net.Socket -> Effect Unit) -> Response 
responseSocket = ResponseSocket