module Netowrk.Http where

-- import Prelude

-- import Data.Int as Int
-- import Data.Map as Map
-- import Data.Maybe (Maybe(..), fromMaybe, maybe)
-- import Foreign.Object as Object
-- import Netowrk.Http.Types (HttpIncoming)
-- import Network.HTTP.Types (http09, http10, http11)
-- import Network.HTTP.Types as H
-- import Network.HTTP.Types.Method as Method
-- import Network.Wai.Internal (RequestBodyLength(..))
-- import Node.Stream (Readable)
-- import Unsafe.Coerce (unsafeCoerce)

-- httpVersion :: HttpIncoming -> Maybe H.HttpVersion
-- httpVersion = parseHttpVersion <<< _.httpVersion <<< unsafeCoerce 
--   where 
--     parseHttpVersion = case _ of 
--       "0.9"     -> pure http09
--       "1.0"     -> pure http10
--       "1.1"     -> pure http11
--       otherwise -> Nothing 

-- url :: HttpIncoming -> String 
-- url = _.url <<< unsafeCoerce 

-- headers :: HttpIncoming -> H.RequestHeaders
-- headers = Object.toUnfoldable <<< _.headers <<< unsafeCoerce 

-- method :: HttpIncoming -> H.Method
-- method = Method.fromString <<< _.method <<< unsafeCoerce

-- contentLength :: HttpIncoming -> RequestBodyLength
-- contentLength = parseContentLength <<< Map.lookup "content-length" <<< Map.fromFoldable <<< headers
--   where 
--     parseContentLength = 
--       maybe ChunkedBody (KnownLength <<< fromMaybe 0 <<< Int.fromString) 

-- bodyStream :: HttpIncoming -> Readable ()
-- bodyStream = unsafeCoerce