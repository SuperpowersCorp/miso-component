{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Miso.Component
       ( Component(Component, app, lens, converter)
       , Updater
       , Converter
       , noReaction
       , initialModel
       , initialAction
       , addInitialAction
       , view
       , subs
       , subMap
       , updater
       , prismify
       )
       where

-- | Miso framework import
import Miso (Effect(Effect), App, View, Sub)
import Miso.Lens (get, set, Lens')
import Data.Maybe (fromMaybe)
import qualified Miso

noReaction :: cAction -> cModel -> pModel -> Effect pAction pModel
noReaction _ _ = return

data Component pAction pModel cAction cModel = Component {
    app       :: App cModel cAction
  , lens      :: Lens' pModel cModel
  , converter :: cAction -> pAction
  }

type Updater pAction pModel = pModel -> Effect pAction pModel

type Converter ca pa = ca -> pa

initialModel :: Component pa pm ca cm -> cm
initialModel = Miso.model . app

initialAction :: Component pa pm ca cm -> pa
initialAction comp = converter comp . Miso.initialAction . app $ comp

addInitialAction :: Effect pa pm -> Component pa pm ca cm -> Effect pa pm
addInitialAction (Effect m axs) comp = Effect m $ ia:axs
  where
    ia = return . converter comp . Miso.initialAction . app $ comp

view :: pm -> Component pa pm ca cm -> View pa
view pm comp = converter comp <$> cview cm
  where
    cm = get (lens comp) pm
    cview = Miso.view . app $ comp

subs :: Component pa pm ca cm -> [Sub pa pm]
subs comp = subMap comp <$> csubs
  where
    csubs = Miso.subs . app $ comp

subMap :: Component pa pm ca cm -> Sub ca cm -> Sub pa pm
subMap comp csub getpm sinkpa =
  csub (get (lens comp) <$> getpm) (sinkpa . converter comp)

updater :: Component pa pm ca cm -> pm -> ca
        -> (ca -> cm -> pm -> Effect pa pm)
        -> Effect pa pm
updater comp pm ca reaction =
  let cm = get (lens comp) pm
      cu = Miso.update . app $ comp
      (Effect cm' caxs) = cu ca cm
      (Effect pm' paxs) = reaction ca cm $ (set (lens comp)) cm' pm
  in
    Effect pm' $ (fmap (converter comp) <$> caxs) ++ paxs

{- A nicer definition if you're using Control.Lens:

import Control.Lens (Prism', review, preview)
import qualified Miso.Component as C

prismify :: Prism' s m -> App m a -> App s a
prismify p = C.prismify (review p) (preview p)

-}

prismify :: (m -> s) -> (s -> Maybe m) -> App m a -> App s a
prismify review preview app' = app'
  { Miso.model = review $ Miso.model app'
  , Miso.update = update'
  , Miso.view = view'
  , Miso.subs = fixsub <$> Miso.subs app'
  }
  where
    update' a pm = case preview pm of
      Just m -> review <$> Miso.update app' a m
      Nothing -> return pm

    view' pm = case preview pm of
      Just m -> Miso.view app' m
      Nothing -> Miso.div_ [] []

    fixsub msub getpm sink =
      msub (fromMaybe (Miso.model app') . preview <$> getpm)
      $ \action -> do
        pm <- getpm
        case preview pm of
          Nothing -> return ()
          Just _ -> sink action
