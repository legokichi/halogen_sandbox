{-
module Util where

import Prelude
import Data.Either (Either(..))
import Data.Foreign.Class (readProp)
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), post)
import Network.HTTP.Affjax.Request (Requestable)

fetch :: forall a eff. (Requestable a)=> String -> a -> Aff (ajax :: AJAX | eff) String
fetch url req = do
  result <- post url req
  let response = result.response
  return response
-}
