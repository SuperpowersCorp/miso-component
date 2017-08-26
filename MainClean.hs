{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import Data.Monoid ((<>))
import qualified Miso.String as Text
--import qualified AlarmClock
import qualified Timer
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens ((^.), (&), (.~), (%~), makeLenses, Lens')
import Miso.Clean (Converter, Updater, Component)
import qualified Miso.Clean as C
--import qualified Miso.Component.Many as Many
--import qualified Miso.Component.Map as CMap

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
  | Init
  | LogMessage Text
  | ComponentUpdater ComponentTag

data ComponentTag
  = Timer1 Timer.Action
  | Timer2 Timer.Action

-- | Entry point for a miso application
main :: IO ()
main = do
  putStrLn "I am a friend of all."
--  Timer.main
  startApp App {..}
  where
    initialAction =  Init
    model  = Model
             (C.initialModel timer1Comp)
             (C.initialModel timer2Comp)
             [ "Welcome to the app."
             , "Start clicking buttons." ]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ keyboardSub $ LogMessage . ms . show]
--             ++ C.subs timer1Comp
--             ++ C.subs timer2Comp

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = return m
updateModel NoOp m = return m
updateModel (LogMessage msg) m = return $ m & messageLog %~ (++[msg])
updateModel (ComponentUpdater ca) m = case ca of
  Timer1 a -> C.updater timer1Comp m a $ \ca _ pm -> case ca of
    Timer.Start -> pm <# return (LogMessage "Timer 1 started.")
    Timer.Buzz -> pm <# return (LogMessage "Timer 1 is buzzing!")
    _ -> return pm

  Timer2 a -> C.updater timer2Comp m a $ \ca _ pm -> case ca of
    Timer.Start -> pm <# return (LogMessage "Timer 2 started.")
    Timer.Buzz -> pm <# return (LogMessage "Timer 2 is buzzing!")
    _ -> return pm

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
   text "Hey there"
 , div_ [] [ text "Some other Timers" ]
 , C.view m timer1Comp
 , C.view m timer2Comp
 , div_ [] $ flip fmap (m ^. messageLog) $ \msg ->
     div_ [] [ text msg ]
 ]


timer1Comp :: Component Action Model Timer.Action Timer.Model
timer1Comp = C.Component {
    app = Timer.app 10
  , converter = ComponentUpdater . Timer1
  , lens = timer1
  }

timer2Comp :: Component Action Model Timer.Action Timer.Model
timer2Comp = C.Component {
    app = Timer.app 20
  , converter = ComponentUpdater . Timer2
  , lens = timer2
  }
