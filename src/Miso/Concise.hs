{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Miso.Concise where

-- | Miso framework import
import Miso (Effect(Effect), App, View, Sub)
import Miso.Lens (get, set, Lens')
import Data.Maybe (fromMaybe)
import qualified Miso

data Interface pAction pModel cAction cModel = Interface {
    lens :: Lens' pModel cModel
  , reaction :: cAction -> cModel -> pModel -> Effect pAction pModel
  }

noReaction :: cAction -> cModel -> pModel -> Effect pAction pModel
noReaction _ _ = return

type UpdateFn action model = action -> model -> Effect action model

data Component pAction pModel cAction cModel = Component {
    app           :: App cModel cAction
  , interface     :: Interface pAction pModel cAction cModel
  , updaterAction :: UpdaterAction pAction pModel
  }

type Converter cAction pAction = cAction -> pAction

type Updater pAction pModel = pModel -> Effect pAction pModel

type UpdaterAction pAction pModel = Updater pAction pModel -> pAction

initialModel :: Component pa pm ca cm -> cm
initialModel = Miso.model . app

initialAction :: Component pa pm ca cm -> pa
initialAction comp = converter comp . Miso.initialAction . app $ comp

view :: pm -> Component pa pm ca cm -> View pa
view pm comp = converter comp <$> cview cm
  where
    cm = get (lens $ interface comp) pm
    cview = Miso.view . app $ comp

subs :: Component pa pm ca cm -> [Sub pa pm]
subs comp = convertSub <$> csubs
  where
    csubs = Miso.subs . app $ comp
    convertSub = subMap (updaterAction comp)
                 (Miso.update . app $ comp)
                 (interface comp)


converter :: Component pa pm ca cm -> Converter ca pa
converter comp = makeConverter
                 (updaterAction comp)
                 (Miso.update . app $ comp)
                 (interface comp)

addInitialAction :: pa -> Component pa pm ca cm -> pa
addInitialAction pa comp = batchActions (updaterAction comp) [pa, initialAction comp]

batchActions :: UpdaterAction action model -> [action] -> action
batchActions ua actions = ua $ \m -> Effect m (return <$> actions)

batchSubs :: [Sub action model] -> Sub action model
batchSubs = foldr combine (\_ _ -> return ())
  where
    combine sub1 sub2 = \getm sink -> sub1 getm sink >> sub2 getm sink

subMap :: UpdaterAction pa pm -> UpdateFn ca cm -> Interface pa pm ca cm -> Sub ca cm
       -> Sub pa pm
subMap ua cu i csub getpm sinkpa =
  csub (get (lens i) <$> getpm) (sinkpa . makeConverter ua cu i)

makeConverter :: UpdaterAction pAction pModel
              -> UpdateFn cAction cModel
              -> Interface pAction pModel cAction cModel
              -> Converter cAction pAction
makeConverter ua cu i ca = ua $ makeUpdater ua cu i ca

makeUpdater :: UpdaterAction pAction pModel
            -> UpdateFn cAction cModel
            -> Interface pAction pModel cAction cModel
            -> cAction
            -> Updater pAction pModel
makeUpdater ua cu i ca pm =
  let cm = get (lens i) pm
      (Effect cm' caxs) = cu ca cm
      (Effect pm' paxs) = (reaction i) ca cm $ (set (lens i)) cm' pm
  in
    Effect pm' $ (fmap (ua . makeUpdater ua cu i) <$> caxs) ++ paxs

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

