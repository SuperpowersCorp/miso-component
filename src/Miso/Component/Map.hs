{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Miso.Component.Map where

import Miso (Effect(Effect), App(App), View, Sub, div_
            , defaultEvents)
import Miso.Lens (get, set, Lens')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Miso
import qualified Miso.Component as C
import Miso.Component ( Component
                      , Converter )

type Model k cm = Map k cm

app :: Ord k => App cm ca -> App (Model k cm) (Action k ca cm)
app capp = App {
    Miso.initialAction = NoOp
  , Miso.model  = Map.empty
  , Miso.update = updateModel capp
  , Miso.view   = viewModel capp
  , Miso.events = defaultEvents
  , Miso.subs   = handleSubs capp
  }

data Action k ca cm
  = Add_ k
  | Add k cm ca
  | Remove_ k
  | Remove ca k
  | SendAction k ca
  | RecvAction k ca cm
  | NoOp

addItem :: Ord k => Model k cm -> k -> cm -> ca -> Effect (Action k ca cm) (Model k cm)
addItem m k cm ca = Effect (Map.insert k cm m) [return $ SendAction k ca]

deleteItem :: Ord k =>  Model k cm -> k -> Model k cm
deleteItem m k = Map.delete k m

updateModel :: Ord k => App cm ca -> Action k ca cm -> Model k cm
            -> Effect (Action k ca cm) (Model k cm)
updateModel app' (Add_ k) m = addItem m k (Miso.model app') (Miso.initialAction app')
updateModel _ (Add k cm ca) m = addItem m k cm ca
updateModel _ (Remove_ k) m = return $ Map.delete k m
updateModel app' (Remove ca k) m = flip (maybe (return $ Map.delete k m))
                                (Map.lookup k m)
                                $ \cm -> do
  let (Effect cm' caxs) = (Miso.update app') ca cm
      m' = Map.delete k m
  Effect m' $ (return $ RecvAction k ca cm')
    :(fmap (SendAction k) <$> caxs)
    --sends out actions, but they don't do anything since nth item is
    --deleted. This is so it can send death messages to parent.
updateModel app' (SendAction k ca) m = flip (maybe $ return m) (Map.lookup k m)
                                       $ \cm -> do
  let (Effect cm' caxs) = (Miso.update app') ca cm
      m' = Map.insert k cm' m
  Effect m' $ (return $ RecvAction k ca cm'):(fmap (SendAction k) <$> caxs)
updateModel _ (RecvAction _k _ca _cm) m = return m
updateModel _ NoOp m = return m

viewModel :: App cm ca -> Model k cm -> View (Action k ca cm)
viewModel app' = div_ [] . fmap viewItem . Map.toList
  where
    viewItem (k, cm) = fmap (SendAction k) $ Miso.view app' cm

handleSubs :: App cm ca -> [Sub (Action k ca cm) (Model k cm)]
handleSubs app' = spreadSub app' <$> Miso.subs app'

-- sinka :: (Action ca cm) -> IO()
spreadSub :: App cm ca -> Sub ca cm -> Sub (Action k ca cm) (Model k cm)
spreadSub app' csub getm sinka = csub getcm spreadSink
  where
    getcm = return $ Miso.model app'
    spreadSink ca = do
      m <- getm
      mapM_ (sinka . flip SendAction ca) $ Map.keys m

add_ :: Component pa pm (Action k ca cm) (Model k cm) -> k -> pa
add_ comp = C.converter comp . Add_

add :: Component pa pm (Action k ca cm) (Model k cm) -> k -> cm -> ca -> pa
add comp k cm = C.converter comp . Add k cm

remove_ :: Component pa pm (Action k ca cm) (Model k cm) -> k -> pa
remove_ comp = C.converter comp . Remove_

remove :: Component pa pm (Action k ca cm) (Model k cm) -> ca -> k -> pa
remove comp finalAction = C.converter comp . Remove finalAction

viewMap :: pm
        -> Component pa pm (Action k ca cm) (Model k cm)
        -> (cm -> View ca)
        -> (k -> cm -> View (Action k ca cm) -> Maybe (View (Action k ca cm)))
        -> Map k (View pa)
viewMap pm comp viewcm wrapper = fmap (C.converter comp)
                                 <$> Map.mapMaybeWithKey viewModel' m
  where
    viewModel' k cm = wrapper k cm (SendAction k <$> viewcm cm)
    m = get (C.lens comp) pm

converter :: Component pa pm (Action k ca cm) (Model k cm) -> k -> Converter ca pa
converter comp k = C.converter comp . SendAction k
