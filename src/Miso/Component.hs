{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Miso.Component where

-- | Miso framework import
import Miso (Effect(Effect), App, View, Sub)
import Miso.Lens (get, set, Lens')
import Miso.Component.Lens ((^.), (&), (.~), (%~))
import qualified Miso

data Interface pAction pModel cAction cModel = Interface {
    lens :: Lens' pModel cModel
  , reaction :: cAction -> cModel -> pModel -> Effect pAction pModel }

noReaction :: cAction -> cModel -> pModel -> Effect pAction pModel
noReaction _ _ = return

type UpdateFn action model = action -> model -> Effect action model

--makeComponent :: (pm -> cm)

data Component pAction pModel cAction cModel = Component {
    app           :: App cModel cAction
  , interface     :: Interface pAction pModel cAction cModel
  , updaterAction :: UpdaterAction pAction pModel
  }

makeComponent :: UpdaterAction pa pm -> Interface pa pm ca cm -> App cm ca
              -> Component pa pm ca cm
makeComponent ua i a = Component { app = a
                                 , interface = i
                                 , updaterAction = ua }

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


{-maybeifyApp :: App model action -> App (model) action
maybeifyApp a = a {
    Miso.model = Just (Miso.model a)
  , 
                }
-}
