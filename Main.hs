{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Map as Map
import Control.Lens ((^.), (&), (.~), (%~), makeLenses, Lens')
import Miso.Component (Converter, Updater, Component)
import qualified Miso.Component as C
import qualified Miso.Component.Many as Many

type Text = MisoString

-- | Type synonym for an application model
data Model = Model {
    _timers :: Many.Model Timer.Model
  , _timer1 :: !Timer.Model
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
  putStrLn "You are a bloated fool. No doubt."
--  Timer.main
  startApp App {..}
  where
    initialAction =  NoOp
                     `C.addInitialAction` timersComp
                     `C.addInitialAction` timer1Comp
                     `C.addInitialAction` timer2Comp
    model  = Model
             (C.initialModel timersComp)
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
             ++ C.subs timersComp

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
 , div_ [] [
       div_ [] [ text "Timers:" ]
     , button_ [ onClick $ Many.add_ timersComp ] [ text "Add a Timer" ] 
     , viewTimers m ]
 , div_ [] $ flip fmap (m ^. messageLog) $ \msg ->
     div_ [] [ text msg ]
 ]

viewTimers :: Model -> View Action
viewTimers m = div_ [] . Map.elems $ Many.viewMap m timersComp Timer.viewModel wrapper
  where
    wrapper n _ v = Just $
      div_ [] [ v
              , button_ [ onClick $ Many.Remove_ n ] [ text "Delete" ] ]

timersComp :: Component Action Model
              (Many.Action Timer.Action Timer.Model)
              (Many.Model Timer.Model)
timersComp = C.Component {
    app = Many.app $ Timer.app 15
  , updaterAction = ComponentUpdater
  , interface = C.Interface
                { lens = timers
                , reaction = C.noReaction
                }
  }

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

