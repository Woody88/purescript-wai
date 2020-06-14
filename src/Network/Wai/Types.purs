module Network.Wai.Types 
    ( Request(..)
    , BaseContext
    , RequestBodyLength(..)
    , Response(..)
    , FilePart(..)
    ) 
    where 

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Network.HTTP.Types as H
import Node.Buffer (Buffer)
import Node.Net.Socket (Socket)
import Node.Stream (Readable)
import Record.Builder (Builder)
import Type.Row (type (+))

newtype Request a ctx = Request (Builder a ctx)

derive newtype instance semigroupoidRequest :: Semigroupoid Request
derive newtype instance categoryRequest :: Category Request

-- type RequestBuilder ctx = Builder {} { | BaseContext + ctx }

-- | Base representation of what is expected from a request.
-- | Is it an open row, therefore, it can be extended.  
type BaseContext ctx = 
    ( url           :: String 
    , contentLength :: RequestBodyLength
    , method        :: H.Method 
    , headers       :: H.RequestHeaders
    | ctx
    )
    
data RequestBodyLength = ChunkedBody | KnownLength Int 

-- | Reprensents all the low level possible ways that a response can be made.
data Response = ResponseString H.Status H.ResponseHeaders String                                       -- Should we allow user to chose encoding?
              | ResponseBuffer H.Status H.ResponseHeaders Buffer
              | ResponseStream H.Status H.ResponseHeaders (Readable ())
              | ResponseSocket (Socket -> Effect Unit)                                                 -- Lowest form of response, user must handle sending headers and status themselves
              | ResponseFile   H.Status H.ResponseHeaders String (Maybe FilePart)
              | ResponseRaw (Effect Buffer -> (Effect Buffer -> Effect Unit) -> Effect Unit) Response  

newtype FilePart = FilePart { offset    :: Int
                            , byteCount :: Int 
                            , fileSize   :: Int
                            }