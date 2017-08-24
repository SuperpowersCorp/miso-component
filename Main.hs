{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Haskell language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import qualified Miso.String as Text
--import qualified AlarmClock
import qualified Timer
import Control.Lens ((^.), (&), (.~), (%~), makeLenses, Lens')
import Miso.Component (Converter, Updater, Component)
import qualified Miso.Component as C

type Text = MisoString

-- | Type synonym for an application model
data Model = Model {
    _timer1 :: !Timer.Model
  , _timer2 :: !Timer.Model
  , _messageLog :: [Text]
  } deriving (Eq, Show)
makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | LogMessage Text
  | ComponentUpdater (Updater Action Model)

-- | Entry point for a miso application
main :: IO ()
main = do
  putStrLn "You're a loser and a complainer."
--  Timer.main
  startApp App {..}
  where
    initialAction = C.batchActions ComponentUpdater
                    [ NoOp
                    , C.initialAction timer1Comp
                    , C.initialAction timer2Comp ]
    model  = Model
             (C.initialModel timer1Comp)
             (C.initialModel timer2Comp)
             [ "Welcome to the app."
             , "Start clicking buttons." ]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ keyboardSub $ LogMessage . ms . show]
             ++ C.subs timer1Comp
             ++ C.subs timer2Comp

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = return m
updateModel (LogMessage msg) m = return $ m & messageLog %~ (++[msg])
updateModel (ComponentUpdater u) m = u m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
   text "Hey there"
 , C.view m timer1Comp
 , C.view m timer2Comp
 , div_ [] $ flip fmap (m ^. messageLog) $ \msg ->
     div_ [] [ text msg ]
 ]

timer1Comp :: Component Action Model Timer.Action Timer.Model
timer1Comp = C.Component {
    app = Timer.app 10
  , updaterAction = ComponentUpdater
  , interface = C.Interface
                { lens = timer1
                , reaction = \ca _ pm -> case ca of
                  Timer.Start -> pm <# return (LogMessage "Timer 1 started.")
                  Timer.Buzz -> pm <# return (LogMessage "Timer 1 is buzzing!")
                  _ -> return pm
                }
  }

timer2Comp :: Component Action Model Timer.Action Timer.Model
timer2Comp = C.Component {
    app = Timer.app 20
  , updaterAction = ComponentUpdater
  , interface = C.Interface
                { lens = timer2
                , reaction = \ca _ pm -> case ca of
                  Timer.Start -> pm <# return (LogMessage "Timer 2 started.")
                  Timer.Buzz -> pm <# return (LogMessage "Timer 2 is buzzing!")
                  _ -> return pm
                }
  }

