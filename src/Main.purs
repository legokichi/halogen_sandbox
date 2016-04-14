{-
module Main where

import Prelude (Unit(), bind, return, unit, ($))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), print, log)
import Control.Monad.Eff.Console.Unsafe (logAny, errorAny)
import Control.Monad.Eff.Console2 (info, warn)
import Control.Monad.Eff.Console2.Unsafe (infoAny, warnAny)
import Control.Monad.Eff.Exception (Error(), EXCEPTION(), throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff (Aff(), runAff)
--import Control.Monad.Aff.Console (print, log)
--import Halogen
--import Halogen.Util (appendToBody, onLoad)
import Data.String.Regex
import Data.Maybe (Maybe(..))
import Data.Array (uncons)
import Data.Int (fromString)
-}
module Main where

import Prelude (Unit(), bind, return, unit, ($), (++), (>>=))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print, log)
import Control.Monad.Eff.Console.Unsafe (logAny, errorAny)
import Control.Monad.Eff.Console2 (info, warn)
import Control.Monad.Eff.Console2.Unsafe (infoAny, warnAny)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans (lift)
import Data.String.Regex (regex, noFlags, match)
import Data.Maybe (Maybe())
import Data.Array (uncons, head, tail, index)
import Data.Int (fromString)
{-
main :: forall eff. Eff ( console :: CONSOLE | eff) Unit
main = do
  num <- runMaybeT $ parseInt "hoge"
  case num of
    Nothing -> do
      log "nothing"
      return unit
    Just a -> do
      log "just"
      print a
  return unit
-}
-- MaybeT :: (* -> *) -> * -> *
-- MaybeT (Eff ()) :: * -> *
-- MaybeT (Eff ()) Int :: *
-- Eff :: # ! -> * -> *
-- () :: # !
-- Eff () :: * -> *
-- Int :: * -- return type
-- runMaybeT :: forall m a. MaybeT m a -> m (Maybe a)
{-parseInt :: forall eff. String -> MaybeT (Eff ( console :: CONSOLE )) (Maybe Int)
parseInt text = do
  let reg = regex "foo(\\d+)" noFlags
  arr <- return $ match reg text
  mstr <- index arr 1
  --str <- mstr
  num <- fromString mstr
  return num
-}

{-
  match reg text >>= \arr　->
  index arr 1 >>= \maybestr ->
  maybestr >>= \str
  fromString str >>= \num ->
  --lift $ log $ "parseInt(" ++ text ++ ") -> " ++ str
  return $ Just num
-}
