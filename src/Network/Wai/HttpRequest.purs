module Network.Wai.HttpRequest where

import Prelude

import Data.Foldable (intercalate)
import Data.Functor (mapFlipped)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Network.HTTP.Types (http09, http10, http11)
import Network.HTTP.Types as Method
import Network.Wai.Internal (class WaiRequest, RequestBodyLength(..), contentLength, headers, host, httpVersion, method, referer, url)
import Node.HTTP as HTTP
import Node.Net.Socket as Socket
import Unsafe.Coerce (unsafeCoerce)

newtype HttpRequest = HttpRequest HTTP.Request

derive instance newtypeHttpRequest :: Newtype HttpRequest _

instance waiRequestHTTP :: WaiRequest HttpRequest where 
    url           = _.url <<< unsafeCoerce <<< unwrap
    method        = Method.fromString <<< _.method <<< unsafeCoerce <<< unwrap
    httpVersion   = parseHttpVersion <<< _.httpVersion <<< unsafeCoerce <<< unwrap
        where 
            parseHttpVersion = case _ of 
                "0.9"     -> http09
                "1.1"     -> http11
                otherwise -> http10

    headers = httpHeaders
    body    = unsafeCoerce <<< unwrap
    host    = fromMaybe mempty <<< Map.lookup "host" <<< Map.fromFoldable <<< httpHeaders
    referer = Map.lookup "referer" <<< Map.fromFoldable <<< httpHeaders
    userAgent = fromMaybe mempty <<< Map.lookup "user-agent" <<< Map.fromFoldable <<< httpHeaders
    remoteHost = remoteHost' <<< _.socket <<< unsafeCoerce <<< unwrap
        where 
            remoteHost' socket = do 
                let mkHost mhost mport = (\host port -> String.joinWith ":" [host, show port]) <$> mhost <*> mport 
                mkHost <$> Socket.remoteAddress socket  <*> Socket.remotePort socket 

    contentLength = parseContentLength <<< Map.lookup "content-length" <<< Map.fromFoldable <<< httpHeaders
      where 
          parseContentLength = 
              maybe ChunkedBody(KnownLength <<< fromMaybe 0 <<< Int.fromString) 

instance showHttpRequest :: Show HttpRequest where 
  show waireq = 
    "Request {" <> (intercalate ", " $ mapFlipped fields (\(a /\ b) -> a <> " = " <> b)) <> "}"
    where
      fields =
        [ "url" /\ (show $ url waireq)
        , "httpVersion" /\ (show $ httpVersion waireq)
        , "method" /\ (show $ method waireq)
        , "headers" /\ (show $ headers waireq)
        , "body" /\ "<ReadableStream>"
        , "host" /\ (show $ host waireq)
        , "remoteHost" /\ "<Effect <Maybe String>>"
        , "referer" /\ (show $ referer waireq)
        , "contentLength" /\ (show $ contentLength waireq)
        ]

httpHeaders :: HttpRequest -> Array (Tuple String String) 
httpHeaders = Object.toUnfoldable <<< _.headers <<< unsafeCoerce <<< unwrap