-- | Unsafe functions to display arbitrary values.
module Control.Monad.Eff.Console2.Unsafe where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE())


foreign import infoAny :: forall a eff. a -> Eff (console :: CONSOLE | eff) Unit


foreign import warnAny :: forall a eff. a -> Eff (console :: CONSOLE | eff) Unit
