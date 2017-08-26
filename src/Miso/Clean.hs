{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Miso.Clean where

-- | Miso framework import
import Miso (Effect(Effect), App, View, Sub)
import Miso.Lens (get, set, Lens')
import Miso.Component.Lens ((^.), (&), (.~), (%~))
import qualified Miso

{-
data Interface pAction pModel cAction cModel = Interface {

  , reaction :: cAction -> cModel -> pModel -> Effect pAction pModel }
-}

noReaction :: cAction -> cModel -> pModel -> Effect pAction pModel
noReaction _ _ = return

type UpdateFn action model = action -> model -> Effect action model

--makeComponent :: (pm -> cm)

data Component pAction pModel cAction cModel = Component {
    app       :: App cModel cAction
  , lens      :: Lens' pModel cModel
  , converter :: cAction -> pAction
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
    cm = get (lens comp) pm
    cview = Miso.view . app $ comp

addInitialAction :: Effect pa pm -> Component pa pm ca cm -> Effect pa pm
addInitialAction (Effect m axs) comp = Effect m $ ia:axs
  where
    ia = return . converter comp . Miso.initialAction . app $ comp

subs :: Component pa pm ca cm -> [Sub pa pm]
subs comp = subMap comp <$> csubs
  where
    csubs = Miso.subs . app $ comp

batchSubs :: [Sub action model] -> Sub action model
batchSubs = foldr combine (\_ _ -> return ())
  where
    combine sub1 sub2 = \getm sink -> sub1 getm sink >> sub2 getm sink

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

