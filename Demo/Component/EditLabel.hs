{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Haskell language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Demo.Component.EditLabel where

import Control.Monad (forever, void)
import Control.Lens ((^.), (&), (.~), (%~), makeLenses)
import Miso
import Miso.String (MisoString, ms)
--import qualified Miso.String as Text
import Control.Concurrent (threadDelay, forkIO)
import Miso.Subscription.Keyboard (keyboardSub)
import qualified Data.Set as Set

type Text = MisoString

-- | Type synonym for an application model
data Model = Model
  { _editing :: Bool
  , _labelVal :: Text
  , _editingVal :: Text
  } deriving (Show, Eq)
makeLenses ''Model

-- | Sum type for application events
data Action
  = Edit
  | Cancel
  | SetLabel Text
  | SetEditVal Text
  | NoOp
  deriving (Show, Eq)

app :: Text -> App Model Action
app startText = App {
      initialAction = NoOp
    , model  = Model False startText startText
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    }

-- | Entry point for a miso application
main :: IO ()
main = startApp $ app ""

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Edit m = return $ m & editing .~ True
                                & editingVal .~ (m ^. labelVal)
updateModel Cancel m = return $ m & editing .~ False
updateModel (SetLabel t) m = return $ m & editing .~ False
                              & labelVal .~ t
updateModel (SetEditVal t) m = return $ m & editingVal .~ t
updateModel NoOp m = return m


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = span_ [] $ case m ^. editing of
  False ->  [ span_ [] [ text $ m ^. labelVal ]
            , button_ [ onClick Edit ] [ text "Edit" ] ]
  True -> [ input_ [ onInput SetEditVal
                   , value_ $ m ^. editingVal ] [ ]
          , button_ [ onClick . SetLabel $ m ^. editingVal ] [ text "Ok" ]
          , button_ [ onClick Cancel ] [ text "Cancel" ] ]
