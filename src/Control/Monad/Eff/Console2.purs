module Control.Monad.Eff.Console2 where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())

foreign import info :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
foreign import warn :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
