{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Miso.Component.Many where

import Miso (Effect(Effect), App(App), View, Sub, div_
            , initialAction, model, update, view, events, subs
            , defaultEvents)
import Miso.Lens (get, set, Lens')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Miso

data Model cm = Model {
    nextId :: Int
  , models :: Map Int cm
  } deriving (Show, Eq)


app :: App cm ca -> App (Model cm) (Action ca cm)
app capp = App {
    initialAction = NoOp
  , model  = Model 0 Map.empty
  , update = updateModel capp
  , view   = viewModel capp
  , events = defaultEvents
  , subs   = handleSubs capp
  }

data Action ca cm
  = Add
  | Add' cm ca
  | Remove Int
  | SendAction Int ca
  | RecvAction Int ca cm
  | NoOp

addItem :: (Model cm) -> cm -> ca -> Effect (Action ca cm) (Model cm)
addItem m cm ca = do
  let n       = nextId m
      models' = models m
      m'      = m { nextId = n + 1
                  , models = Map.insert n cm models' }
  Effect m' [return $ SendAction n ca]

updateModel :: App cm ca -> Action ca cm -> Model cm
            -> Effect (Action ca cm) (Model cm)
updateModel app' Add m = addItem m (model app') (initialAction app')
updateModel _ (Add' cm ca) m = addItem m cm ca
updateModel _ (Remove n) m = return $ m {
  models = Map.delete n $ models m }
updateModel app' (SendAction n ca) m = flip (maybe $ return m) (Map.lookup n (models m))
                                       $ \cm -> do
  let (Effect cm' caxs) = (update app') ca cm
      m' = m { models = Map.insert n cm $ models m }
  Effect m' $ (return $ RecvAction n ca cm'):(fmap (SendAction n) <$> caxs)
updateModel _ (RecvAction _n _ca _cm) m = return m
updateModel _ NoOp m = return m

viewModel :: App cm ca -> Model cm -> View (Action ca cm)
viewModel app' = div_ [] . fmap viewItem . Map.toList . models
  where
    viewItem (n, cm) = fmap (SendAction n) $ view app' cm

handleSubs :: App cm ca -> [Sub (Action ca cm) (Model cm)]
handleSubs app' = []
