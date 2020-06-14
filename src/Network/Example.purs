module Network.Example where

import Prelude

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Network.HTTP.Types (status200)
import Network.Wai (Application, Middleware, BaseContext, getRequest, request, responseStr)
import Node.HTTP as Http
import Node.HTTP.Client as HTTP
import Prim.Row as Row
import Record.Builder as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type WarpContext r = BaseContext + HttpRequest + r
type HttpRequest r = (httpRequest :: Http.Request | r)
type ReqContext r = (x :: String | r)

reqMiddleware :: ∀ ctx. Row.Lacks "x" ctx ⇒ Middleware (ReqContext ctx) ctx  
reqMiddleware apps req send = apps (req2 <<< req) send 
  where   
    req2 = request (Record.insert (SProxy :: SProxy "x") "hello")

app :: Application _
app req' send = do
  let req = getRequest req'
      n = req.httpRequest
  Console.log $ req.x
  send $ responseStr status200 [] "heellox"

run :: Int -> Application (WarpContext ()) -> Aff Unit 
run i app3 = unsafeCoerce ""

main :: Effect Unit 
main = launchAff_ do run 8080 $ reqMiddleware app