module Network.Example where

import Prelude

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Network.HTTP.Types (status200)
import Network.Wai (Application, Middleware, getRequest, request, responseStr)
import Prim.Row as Row
import Record.Builder as Record
import Unsafe.Coerce (unsafeCoerce)

reqMiddleware :: ∀ ctx. Row.Lacks "x" ctx ⇒ Middleware (x :: String | ctx) ctx  
reqMiddleware apps req send = apps (req2 <<< req) send 
  where   
    req2 = request (Record.insert (SProxy :: SProxy "x") "hello")

app :: Application (x :: String)
app req' send = do
  let req = getRequest req'
  Console.log $ req.x
  send $ responseStr status200 [] "heellox"

run :: forall r. Int -> Application r -> Aff Unit 
run i app3 = unsafeCoerce ""

main :: Effect Unit 
main = launchAff_ do run 8080 $ reqMiddleware app