{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Demo.Prism where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import Data.Monoid ((<>))
import qualified Miso.String as Text
--import qualified AlarmClock
import qualified Timer
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens ((^.), (&), (.~), (%~), makeLenses, Lens'
                    , makePrisms, preview)
import Miso.Clean (Converter, Updater, Component)
import qualified Miso.Clean as C
--import qualified Miso.Component.Many as Many
import qualified Miso.Component.CleanMap as CMap
import Miso.Lens ( makeLens )

type Text = MisoString

data PageChoice = LoggedOut
                | PageTimer1 Timer.Model
                | PageTimer2 Timer.Model
                deriving (Eq, Show)

makePrisms ''PageChoice

-- | Type synonym for an application model
data Model = Model {
    _page :: PageChoice
  , _messageLog :: [Text]
  } deriving (Eq, Show)
makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | Init
  | LogMessage Text
  | LoginTimer1
  | LoginTimer2
  | Logout
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
             LoggedOut
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
updateModel Init m = return m
updateModel NoOp m = return m
updateModel LoginTimer1 m = (return $ m & page .~ C.initialModel timer1Comp)
                            `C.addInitialAction` timer1Comp
updateModel LoginTimer2 m = (return $ m & page .~ C.initialModel timer2Comp)
                            `C.addInitialAction` timer2Comp
updateModel Logout m = return $ m & page .~ LoggedOut
updateModel (LogMessage msg) m = return $ m & messageLog %~ (++[msg])
updateModel (ComponentUpdater ca) m = case ca of
  Timer1 a -> C.updater timer1Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (LogMessage "Timer 1 started.")
    Timer.Buzz -> m' <# return (LogMessage "Timer 1 is buzzing!")
    _ -> return m'

  Timer2 a -> C.updater timer2Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (LogMessage "Timer 2 started.")
    Timer.Buzz -> m' <# return (LogMessage "Timer 2 is buzzing!")
    _ -> return m'

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
  case m ^. page of
  LoggedOut -> div_ [] [ text "You're not logged in, idiot."
                      , button_ [ onClick LoginTimer1 ] [ text "Login to Timer1" ]
                      , button_ [ onClick LoginTimer2 ] [ text "Login to Timer2" ] ]
  PageTimer1 _ -> div_ [] [ div_ [] [ text "Timer 1:" ]
                          , C.view m timer1Comp
                          , button_ [ onClick Logout ] [ text "Logout" ] ]
  PageTimer2 _ -> div_ [] [ div_ [] [ text "Timer 1:" ]
                          , C.view m timer2Comp
                          , button_ [ onClick Logout ] [ text "Logout" ] ]
  , div_ [] $ flip fmap (m ^. messageLog) $ \msg ->
    div_ [] [ text msg ]
  ]


timer1Comp :: Component Action Model Timer.Action PageChoice
timer1Comp = C.Component {
    app = C.prismify _PageTimer1 $ Timer.app 10
  , converter = ComponentUpdater . Timer1
  , lens = page
  }

timer2Comp :: Component Action Model Timer.Action PageChoice
timer2Comp = C.Component {
    app = C.prismify _PageTimer2 $ Timer.app 20
  , converter = ComponentUpdater . Timer2
  , lens = page
  }
