{-# LANGUAGE TemplateHaskell #-}
-- | Haskell language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Tester where

import Control.Lens ((^.), (&), (.~), (%~), makeLenses)
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
  , _someInt :: Int
  } deriving (Show, Eq)
makeLenses ''Model

-- | Sum type for application events
data Action
  = Tick UTCTime
  | Buzz
  | Delayed Int Action
  | ModifyInt (Int -> Int)
  | StopBuzz
  | NoOp

-- | Entry point for a miso application
main :: IO ()
main = do
  ct <- getCurrentTime
  startApp App {
      initialAction = Tick ct
    , model  = Model ct Nothing False 0
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
updateModel (Delayed n a) m = m <# do
  _ <- threadDelay n
  return a
updateModel (ModifyInt f) m = return $ m & someInt %~ f
updateModel Buzz m = do
  m' <- updateModel (ModifyInt (+1)) m
  m'' <- updateModel (Delayed 3000000 (ModifyInt (+5))) m'
  m''' <- updateModel (ModifyInt (+1)) m''
  return m'''
updateModel StopBuzz m = return $ m & someInt .~ 0
updateModel NoOp m = return m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] $ concat [
    [ div_ [] [ text . ms . show $ m ^. currentTime ] ]
  , [ button_ [ onClick Buzz ] [ text "Start the count up!" ] ]
  , [ button_ [ onClick StopBuzz ] [ text "Reset to zero" ] ]
  , [ div_ [] [ text . ms . show $ m ^. someInt ] ]
  ]
