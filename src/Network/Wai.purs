module Network.Wai 
    ( Application 
    , Middleware (..)
    , module Network.Wai.Types
    , request
    , getRequest
    , responseFile
    , responseStr
    , responseStream
    , responseSocket
    , responseRaw
    ) where 

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Network.HTTP.Types (Status, ResponseHeaders)
import Network.Wai.Types (FilePart(..), Request(..), RequestBodyLength(..), Response(..), BaseContext)
import Node.Buffer (Buffer)
import Node.Net.Socket as Net
import Node.Path (FilePath)
import Node.Stream as Stream
import Record.Builder (Builder)
import Record.Builder as Record
import Type.Row (type (+))

type Application ctx =  Request {} { | BaseContext + ctx} -> (Response -> Aff Unit) -> Aff Unit

type Middleware ctx' ctx = Application ctx' -> Application ctx

request :: forall from ctx. Builder from ctx -> Request from ctx
request = Request

getRequest :: forall ctx. Request {} { | ctx } -> { | ctx}
getRequest (Request b) = Record.build b {}

-- | Creating 'Response' from a string
responseStr :: Status -> ResponseHeaders -> String -> Response
responseStr = ResponseString 

-- | Creating 'Response' from a file.
responseFile :: Status -> ResponseHeaders -> FilePath -> Maybe FilePart -> Response 
responseFile = ResponseFile 

-- | Creating 'Response' from a duplex stream 
responseStream :: Status -> ResponseHeaders -> Stream.Readable () -> Response 
responseStream = ResponseStream

-- | Creating 'Response' from a socket
-- | Lowest form of response, user must handle sending headers and status themselves
responseSocket :: (Net.Socket -> Effect Unit) -> Response 
responseSocket = ResponseSocket

-- | Create a response for a raw application. 
-- | This represents a stream as a function and accepts a Response as a backup in case of failure
-- | The first parameter `Effect Buffer` is the receive handler of the stream
-- | The second parameter `(Effect Buffer -> Effect Unit)` is the sending handler of the stream
responseRaw :: (Effect Buffer -> (Effect Buffer -> Effect Unit) -> Effect Unit) -> Response -> Response 
responseRaw = ResponseRaw