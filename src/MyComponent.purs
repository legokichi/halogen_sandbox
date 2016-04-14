{-
module MyComponent where

import Prelude (Unit(), bind, const, flip, pure, return, unit, ($), (++))


import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (throwException)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P
import Network.HTTP.Affjax (AJAX())


-- | The state of the component.
type State = { busy :: Boolean, result :: Maybe String }

initialState :: State
initialState = { busy: false, result: Nothing }

-- | The component query algebra.
data Query a
  = SetCode String a
  | MakeRequest String a

-- | The effects used in the app.
-- type HalogenEffects eff = (avar :: AVAR, err :: EXCEPTION, dom :: DOM | eff)
type AppEffects eff = HalogenEffects (ajax :: AJAX| eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_ $
      [ H.h1_
          [ H.text "ajax example / trypurescript" ]
      , H.h2_
          [ H.text "purescript input:" ]
      ]
      ++ flip foldMap st.result \js ->
          [ H.div_
              [ H.h2_
                  [ H.text "javascript output:" ]
              , H.pre_
                  [ H.code_ [ H.text js ] ]
              ]
          ]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  eval (SetCode code next) = modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (MakeRequest code next) = do
    modify (_ { busy = true })
    result <- liftAff' (fetchJS code)
    modify (_ { busy = false, result = Just result })
    pure next


-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
-}
