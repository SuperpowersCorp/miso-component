{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Miso.Component.Many where

import Miso (Effect(Effect), App(App), View, Sub, div_
            , defaultEvents)
import Miso.Lens (get, set, Lens')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Miso
import qualified Miso.Component as C
import Miso.Component (Component)

data Model cm = Model {
    nextId :: Int
  , models :: Map Int cm
  } deriving (Show, Eq)

app :: App cm ca -> App (Model cm) (Action ca cm)
app capp = App {
    Miso.initialAction = NoOp
  , Miso.model  = Model 0 Map.empty
  , Miso.update = updateModel capp
  , Miso.view   = viewModel capp
  , Miso.events = defaultEvents
  , Miso.subs   = handleSubs capp
  }

data Action ca cm
  = Add_
  | Add cm ca
  | Remove_ Int
  | Remove ca Int
  | SendAction Int ca
  | RecvAction Int ca cm
  | NoOp

addItem :: Model cm -> cm -> ca -> Effect (Action ca cm) (Model cm)
addItem m cm ca = do
  let n       = nextId m
      models' = models m
      m'      = m { nextId = n + 1
                  , models = Map.insert n cm models' }
  Effect m' [return $ SendAction n ca]

deleteItem :: Model cm -> Int -> Model cm
deleteItem m n = m { models = Map.delete n $ models m }

updateModel :: App cm ca -> Action ca cm -> Model cm
            -> Effect (Action ca cm) (Model cm)
updateModel app' Add_ m = addItem m (Miso.model app') (Miso.initialAction app')
updateModel _ (Add cm ca) m = addItem m cm ca
updateModel _ (Remove_ n) m = return $ m {
  models = Map.delete n $ models m }
updateModel app' (Remove ca n) m = flip (maybe (return $ deleteItem m n))
                                (Map.lookup n (models m))
                                $ \cm -> do
  let (Effect cm' caxs) = (Miso.update app') ca cm
      m' = deleteItem m n
  Effect m' $ (return $ RecvAction n ca cm')
    :(fmap (SendAction n) <$> caxs)
    --sends out actions, but they don't do anything since nth item is
    --deleted. This is so it can send death messages to parent.
updateModel app' (SendAction n ca) m = flip (maybe $ return m) (Map.lookup n (models m))
                                       $ \cm -> do
  let (Effect cm' caxs) = (Miso.update app') ca cm
      m' = m { models = Map.insert n cm' $ models m }
  Effect m' $ (return $ RecvAction n ca cm'):(fmap (SendAction n) <$> caxs)
updateModel _ (RecvAction _n _ca _cm) m = return m
updateModel _ NoOp m = return m

viewModel :: App cm ca -> Model cm -> View (Action ca cm)
viewModel app' = div_ [] . fmap viewItem . Map.toList . models
  where
    viewItem (n, cm) = fmap (SendAction n) $ Miso.view app' cm

handleSubs :: App cm ca -> [Sub (Action ca cm) (Model cm)]
handleSubs app' = spreadSub app' <$> Miso.subs app'

-- sinka :: (Action ca cm) -> IO()
spreadSub :: App cm ca -> Sub ca cm -> Sub (Action ca cm) (Model cm)
spreadSub app' csub getm sinka = csub getcm spreadSink
  where
    getcm = return $ Miso.model app'
    spreadSink ca = do
      m <- getm
      mapM_ (sinka . flip SendAction ca) (Map.keys $ models m)

add_ :: Component pa pm (Action ca cm) (Model cm) -> pa
add_ comp = C.converter comp $ Add_

add :: Component pa pm (Action ca cm) (Model cm) -> cm -> ca -> pa
add comp cm = C.converter comp . Add cm

remove_ :: Component pa pm (Action ca cm) (Model cm) -> Int -> pa
remove_ comp = C.converter comp . Remove_

remove :: Component pa pm (Action ca cm) (Model cm) -> ca -> Int -> pa
remove comp finalAction = C.converter comp . Remove finalAction

viewMap :: pm
        -> Component pa pm (Action ca cm) (Model cm)
        -> (cm -> View ca)
        -> (Int -> cm -> View (Action ca cm) -> Maybe (View (Action ca cm)))
        -> Map Int (View pa)
viewMap pm comp viewcm wrapper = fmap (C.converter comp)
                                 <$> Map.mapMaybeWithKey viewModel' models'
  where
    viewModel' n cm = wrapper n cm (SendAction n <$> viewcm cm)
    models' = models $ get (C.lens . C.interface $ comp) pm

