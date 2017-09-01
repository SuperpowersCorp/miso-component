{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Demo.Map where

-- | Miso framework import
import Miso
import Miso.String (MisoString)
import Data.Monoid ((<>))
import qualified Demo.Component.Timer as Timer
import qualified Demo.Component.EditLabel as Label
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens ((^.), makeLenses)
import Miso.Component (Component)
import qualified Miso.Component as C
import qualified Miso.Component.Map as CMap

type Text = MisoString

-- | Type synonym for an application model
data Model = Model {
    _label :: Label.Model
  , _mtimers :: Map Text Timer.Model
  } deriving (Eq, Show)
makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | Init
  | Log Text
  | ComponentUpdater ComponentTag

data ComponentTag
  = Label Label.Action
  | MTimers (CMap.Action Text Timer.Action Timer.Model)

app :: App Model Action
app = App {..}
  where
    initialAction =  Init
    model  = Model
             (C.initialModel labelComp)
             Map.empty
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = C.subs labelComp ++ C.subs mtimersComp

-- | Entry point for a miso application
main :: IO ()
main = startApp app

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = (Effect m $ fmap return [ CMap.add_ mtimersComp "Jones"
                                             , CMap.add_ mtimersComp "Angel-Puff"
                                             , CMap.add_ mtimersComp "Willy Winkins" ])
                     `C.addInitialAction` labelComp
                     `C.addInitialAction` mtimersComp
updateModel NoOp m = return m
updateModel (Log _) m = return m
updateModel (ComponentUpdater ca) m = case ca of
  Label a -> C.updater labelComp m a C.noReaction

  MTimers a -> C.updater mtimersComp m a $ \ca' _ m' -> case ca' of
    CMap.RecvAction k ta _tm ->
      case ta of
      Timer.Buzz -> m' <# do
        return . Log $ k <> " is BUZZZING!!!!!!"
      _ -> return m'
    _ -> return m'

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ []
 [ div_ [] [ C.view m labelComp
           , let newName = m ^. (C.lens labelComp) . Label.labelVal in
             if Map.member newName (m ^. (C.lens mtimersComp))
               then span_ [] [ text $ "\"" <> newName <> "\" has been added."]
               else button_ [ onClick $ CMap.add_ mtimersComp newName ]
                    [ text $ "Add a new timer named \"" <> newName <> "\"" ] ]
 , h3_ [] [ text "Your Timer Collection" ]
 , div_ [] [ text "Press Space-Bar to toggle all timers." ]
 , div_ [] [ viewMTimers $ m ^. mtimers ]
 ]


viewMTimers :: Map Text Timer.Model -> View Action
viewMTimers = div_ [] . fmap vt . Map.toList
  where
    vt (k, tm) = div_ [] [ h4_ [] [ text k ]
                         , CMap.converter mtimersComp k <$> Timer.viewModel tm
                         , button_ [ onClick $ CMap.remove_ mtimersComp k ]
                           [ text $ "Delete \"" <> k <> "\" Timer" ] ]

labelComp :: Component Action Model Label.Action Label.Model
labelComp = C.Component {
    app = Label.app "NewTimer"
  , converter = ComponentUpdater . Label
  , lens = label
  }

mtimersComp :: Component Action Model
              (CMap.Action Text Timer.Action Timer.Model)
              (Map Text Timer.Model)
mtimersComp = C.Component {
    app = CMap.app $ Timer.app 15
  , converter = ComponentUpdater . MTimers
  , lens = mtimers
  }
