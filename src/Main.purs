module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (text)
import Specular.Dom.Widget (class MonadWidget, runMainWidgetInBody)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.Dom.Widgets.Input (textInputOnInput)
import Specular.FRP (Dynamic, Event, dynamic, fixFRP, holdDyn, never, switch)
import Specular.FRP.Base (filterMapEvent, tagDyn)

main :: Eff (infinity :: INFINITY) Unit
main = runIOSync $ runMainWidgetInBody mainWidget

mainWidget :: forall m. MonadWidget m => m Unit
mainWidget = void $
  withNextButton stage1 `chain`
  (withNextButton <<< stage2) `chain`
  (withNextButton <<< stage3) `chain`
  finalStage

type Stage1 = { firstName :: String }
type Stage2 = { firstName :: String, lastName :: String }
type Stage3 = { firstName :: String, lastName :: String, nickname :: String }

stage1 :: forall m. MonadWidget m => m (Dynamic Stage1)
stage1 = do
  text "Enter first name"
  firstNameD <- textInputOnInput "" mempty
  pure $ firstNameD <#> \firstName -> { firstName }

stage2 :: forall m. MonadWidget m => Stage1 -> m (Dynamic Stage2)
stage2 { firstName } = do
  text "Enter last name"
  lastNameD <- textInputOnInput "" mempty
  pure $ lastNameD <#> \lastName -> { firstName, lastName }

stage3 :: forall m. MonadWidget m => Stage2 -> m (Dynamic Stage3)
stage3 { firstName, lastName } = do
  text "Enter nickname"
  nicknameD <- textInputOnInput "" mempty
  pure $ nicknameD <#> \nickname -> { firstName, lastName, nickname }

finalStage :: forall m. MonadWidget m => Stage3 -> m (Event Unit)
finalStage finalData = do
  text $ "Final data: firstName=" <> finalData.firstName <> ", lastName=" <> finalData.lastName <> ", nickname=" <> finalData.nickname
  pure never

-- | Given a widget returning a Dynamic value, adorn it with "Next" button.
-- | Returns an Event which fires when the button is clicked, with the returned
-- | value of the inner widget.
withNextButton :: forall m a. MonadWidget m => m (Dynamic a) -> m (Event a)
withNextButton inner = do
  value <- inner
  next <- buttonOnClick (pure mempty) (text "Next")
  pure (tagDyn value next)

-- | Display the first widget. When the returned event fires, switch to the
-- | second widget, passing it the event value.
chain :: forall m a b. MonadWidget m => m (Event a) -> (a -> m (Event b)) -> m (Event b)
chain first second =
  fixFRP \fired' -> do
    (currentWidget :: Dynamic (m (Event (Either a b)))) <-
      holdDyn
        (map Left <$> first)
          -- start with the first widget, tagging events with Left
        ((map (map Right) <<< second) <$> filterMapEvent fromLeftM fired')
          -- when the first widget fires, switch to the second one, tagging events with Right
    (fired :: Event (Either a b)) <- map switch $ dynamic currentWidget
    pure $ Tuple fired (filterMapEvent fromRightM fired)

fromLeftM :: forall e a. Either e a -> Maybe e
fromLeftM (Left x) = Just x
fromLeftM (Right _) = Nothing

fromRightM :: forall e a. Either e a -> Maybe a
fromRightM (Left _) = Nothing
fromRightM (Right x) = Just x
