{-# LANGUAGE TemplateHaskell #-}
-- | Haskell language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module AlarmClock where

import Control.Lens ((^.), (&), (.~), makeLenses)
import Miso
import Miso.String (MisoString, ms)
--import qualified Miso.String as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent (threadDelay)

type Text = MisoString

-- | Type synonym for an application model
data Model = Model {
    _currentTime :: UTCTime
  , _alarmTime :: Maybe UTCTime
  , _buzzing :: Bool
  } deriving (Show, Eq)
makeLenses ''Model

-- | Sum type for application events
data Action
  = Tick UTCTime
  | Buzz
  | StopBuzz
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  ct <- getCurrentTime
  startApp App {
      initialAction = Tick ct
    , model  = Model ct Nothing False
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (Tick ct) m = (m & currentTime .~ ct) <# do
  _ <- threadDelay 1000000
  ct' <- getCurrentTime
  return $ Tick ct'
updateModel StopBuzz m = noEff m
updateModel Buzz m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] $ concat [
    [ div_ [] [ text . ms . show $ m ^. currentTime ] ]
  , [ button_ [ onClick StopBuzz ] [ text "Stop the BuzzZZZ" ] ]
  ]
