{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (MisoString, ms)
import Data.Monoid ((<>))
import qualified Miso.String as Text
--import qualified AlarmClock
import qualified Timer
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens ((^.), (&), (.~), (%~), makeLenses, Lens'
                    , makePrisms, preview)
import Miso.Clean (Converter, Updater, Component)
import qualified Miso.Clean as C
--import qualified Miso.Component.Many as Many
import qualified Miso.Component.CleanMap as CMap
import Miso.Lens ( makeLens )

type Text = MisoString

data PageChoice = LoggedOut
                | PageTimer1 Timer.Model
                | PageTimer2 Timer.Model
                deriving (Eq, Show)

makePrisms ''PageChoice

-- | Type synonym for an application model
data Model = Model {
    _page :: PageChoice
  , _mtimers :: Map Text Timer.Model
  , _messageLog :: [Text]
  } deriving (Eq, Show)
makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | Init
  | LogMessage Text
  | Login
  | ComponentUpdater ComponentTag

data ComponentTag
  = Timer1 Timer.Action
  | Timer2 Timer.Action
  | MTimers (CMap.Action Text Timer.Action Timer.Model)

-- | Entry point for a miso application
main :: IO ()
main = do
  putStrLn "I am a friend of all."
--  Timer.main
  startApp App {..}
  where
    initialAction =  Init
    model  = Model
             LoggedOut
             Map.empty
             [ "Welcome to the app."
             , "Start clicking buttons." ]
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = [ keyboardSub $ LogMessage . ms . show]
             ++ C.subs timer1Comp
             ++ C.subs timer2Comp
             ++ C.subs mtimersComp

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = (Effect m $ fmap return [ CMap.add_ mtimersComp "Jones"
                                             , CMap.add_ mtimersComp "Angel-Puff"
                                             , CMap.add_ mtimersComp "Willy Winkins" ])
                     `C.addInitialAction` timer1Comp
                     `C.addInitialAction` timer1Comp
                     `C.addInitialAction` mtimersComp
updateModel NoOp m = return m
updateModel (LogMessage msg) m = return $ m & messageLog %~ (++[msg])
updateModel (ComponentUpdater ca) m = case ca of
  Timer1 a -> C.updater timer1Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (LogMessage "Timer 1 started.")
    Timer.Buzz -> m' <# return (LogMessage "Timer 1 is buzzing!")
    _ -> return m'

  Timer2 a -> C.updater timer2Comp m a $ \ca' _ m' -> case ca' of
    Timer.Start -> m' <# return (LogMessage "Timer 2 started.")
    Timer.Buzz -> m' <# return (LogMessage "Timer 2 is buzzing!")
    _ -> return m'

  MTimers a -> C.updater mtimersComp m a $ \ca' _ m' -> case ca' of
    CMap.RecvAction k ta _tm ->
      case ta of
      Timer.Buzz -> m' <# do
        putStrLn $ show k ++ " is BUZZZING!!!!!!"
        return NoOp
      _ -> return m'
    _ -> return m'

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] [
  case m ^. page of
  LoggedOut -> div_ [] [ text "You're not logged in yet, idiot."
                      , button [ onClick Login ] [ text "Login to Timer1" ]
                      , button [ onClick Login ] [ text "Login to Timer1" ] ]
  PageTimer1 _ -> div_ [] [ div_ [] [ text "Timer 1:" ]
                          , C.view m timer1Comp ]
  PageTimer2 _ -> div_ [] [ div_ [] [ text "Timer 1:" ]
                          , C.view m timer2Comp ]


viewMTimers :: Map Text Timer.Model -> View Action
viewMTimers = div_ [] . fmap vt . Map.toList
  where
    vt (k, tm) = div_ [] [ CMap.converter mtimersComp k <$> Timer.viewModel tm
                         , button_ [ onClick $ CMap.remove_ mtimersComp k ]
                           [ text $ "Delete \"" <> k <> "\" Timer" ] ]

timer1Comp :: Component Action Model Timer.Action (Maybe Timer.Model)
timer1Comp = C.Component {
    app = C.maybeify $ Timer.app 10
  , converter = ComponentUpdater . Timer1
  , lens = makeLens (\m -> preview _PageTimer1 $ m ^. page)
           (\m mcm -> flip (maybe m) mcm $ \cm ->
             case m ^. page of
             PageTimer1 _ -> m & page .~ PageTimer1 cm
             _ -> m)
  }

timer2Comp :: Component Action Model Timer.Action (Maybe Timer.Model)
timer2Comp = C.Component {
    app = C.maybeify $ Timer.app 20
  , converter = ComponentUpdater . Timer2
  , lens =  makeLens (\m -> preview _PageTimer2 $ m ^. page)
            (\m mcm -> flip (maybe m) mcm $ \cm ->
             case m ^. page of
             PageTimer2 _ -> m & page .~ PageTimer2 cm
             _ -> m)
  }

mtimersComp :: Component Action Model
              (CMap.Action Text Timer.Action Timer.Model)
              (Map Text Timer.Model)
mtimersComp = C.Component {
    app = CMap.app $ Timer.app 15
  , converter = ComponentUpdater . MTimers
  , lens = mtimers
  }
