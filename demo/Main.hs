{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString)
import Data.Monoid ((<>))
--import qualified AlarmClock
import Control.Lens ((^.), (&), (%~), makeLenses)
import Miso.Component (Component)
import qualified Miso.Component as C
import qualified Demo.Map as DemoMap
import qualified Demo.Prism as DemoPrism

type Text = MisoString

-- | Type synonym for an application model
data Model = Model
  { _demoPrism :: DemoPrism.Model
  , _demoMap :: DemoMap.Model
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
  = DemoPrism DemoPrism.Action
  | DemoMap DemoMap.Action

-- | Entry point for a miso application
main :: IO ()
main = do
  putStrLn "I am a friend of all."
--  Timer.main
  startApp App {..}
  where
    initialAction =  Init
    model  = Model
             (C.initialModel demoPrismComp)
             (C.initialModel demoMapComp)
             [ "Welcome to the app."
             , "Start clicking buttons." ]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = C.subs demoPrismComp
             ++ C.subs demoMapComp

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = return m
                     `C.addInitialAction` demoPrismComp
                     `C.addInitialAction` demoMapComp
updateModel NoOp m = return m
updateModel (LogMessage msg) m = return $ m & messageLog %~ (++[msg])
updateModel (ComponentUpdater ca) m = case ca of
  DemoPrism a -> C.updater demoPrismComp m a $ \ca' _ m' -> case ca' of
    DemoPrism.Log t -> m' <# return (LogMessage $ "DemoPrism: " <> t)
    _ -> return m'

  DemoMap a -> C.updater demoMapComp m a $ \ca' _ m' -> case ca' of
    DemoMap.Log t -> m' <# return (LogMessage $ "DemoMap: " <> t)
    _ -> return m'

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ []
  [ h2_ [] [ text "Demo.Prism :" ]
  , C.view m demoPrismComp
  , h2_ [] [ text "Demo.Map :" ]
  , C.view m demoMapComp
  , h1_ [] [ text "Log:" ]
  , div_ [] $ flip fmap (m ^. messageLog) $ \msg ->
    div_ [] [ text msg ]
  ]


demoPrismComp :: Component Action Model DemoPrism.Action DemoPrism.Model
demoPrismComp = C.Component {
    app = DemoPrism.app
  , converter = ComponentUpdater . DemoPrism
  , lens = demoPrism
  }


demoMapComp :: Component Action Model DemoMap.Action DemoMap.Model
demoMapComp = C.Component {
    app = DemoMap.app
  , converter = ComponentUpdater . DemoMap
  , lens = demoMap
  }
