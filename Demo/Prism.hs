{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Demo.Prism where

-- | Miso framework import
import Miso
import Miso.String (MisoString)
import qualified Demo.Component.Timer as Timer
import Control.Lens ((^.), (&), (.~), makeLenses
                    , makePrisms, preview, review, Prism')
import Miso.Component (Component)
import qualified Miso.Component as C
--import qualified Miso.Component.Many as Many

type Text = MisoString

data PageChoice = LoggedOut
                | PageTimer1 Timer.Model
                | PageTimer2 Timer.Model
                deriving (Eq, Show)

makePrisms ''PageChoice

-- | Type synonym for an application model
data Model = Model {
    _page :: PageChoice
  } deriving (Eq, Show)
makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | Init
  | Log Text
  | LoginTimer1
  | LoginTimer2
  | Logout
  | ComponentUpdater ComponentTag

data ComponentTag
  = Timer1 Timer.Action
  | Timer2 Timer.Action

app :: App Model Action
app = App {..}
  where
    initialAction =  Init
    model  = Model
             LoggedOut
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = C.subs timer1Comp ++ C.subs timer2Comp

-- | Entry point for a miso application
main :: IO ()
main = startApp app

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = return m
updateModel NoOp m = return m
updateModel LoginTimer1 m = (return $ m & page .~ C.initialModel timer1Comp)
                            `C.addInitialAction` timer1Comp
updateModel LoginTimer2 m = (return $ m & page .~ C.initialModel timer2Comp)
                            `C.addInitialAction` timer2Comp
updateModel Logout m = return $ m & page .~ LoggedOut
updateModel (Log _) m = return m
updateModel (ComponentUpdater ca) m = case ca of
  Timer1 a -> C.updater timer1Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (Log "Timer 1 started.")
    Timer.Buzz -> m' <# return (Log "Timer 1 is buzzing!")
    _ -> return m'

  Timer2 a -> C.updater timer2Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (Log "Timer 2 started.")
    Timer.Buzz -> m' <# return (Log "Timer 2 is buzzing!")
    _ -> return m'

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
  case m ^. page of
  LoggedOut -> div_ [] [ text "You're not logged in."
                      , button_ [ onClick LoginTimer1 ] [ text "Login to Timer1" ]
                      , button_ [ onClick LoginTimer2 ] [ text "Login to Timer2" ] ]
  PageTimer1 _ -> div_ [] [ div_ [] [ text "Timer 1:" ]
                          , C.view m timer1Comp
                          , button_ [ onClick Logout ] [ text "Logout" ] ]
  PageTimer2 _ -> div_ [] [ div_ [] [ text "Timer 2:" ]
                          , C.view m timer2Comp
                          , button_ [ onClick Logout ] [ text "Logout" ] ]
  ]


prismify :: Prism' s m -> App m a -> App s a
prismify p = C.prismify (review p) (preview p)

timer1Comp :: Component Action Model Timer.Action PageChoice
timer1Comp = C.Component {
    app = prismify _PageTimer1 $ Timer.app 10
  , converter = ComponentUpdater . Timer1
  , lens = page
  }

timer2Comp :: Component Action Model Timer.Action PageChoice
timer2Comp = C.Component {
    app = prismify _PageTimer2 $ Timer.app 20
  , converter = ComponentUpdater . Timer2
  , lens = page
  }
