{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Haskell language pragmas
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Timer where

import Control.Monad (forever, void)
import Control.Lens ((^.), (&), (.~), (%~), makeLenses)
import Miso
import Miso.String (MisoString, ms)
--import qualified Miso.String as Text
import Control.Concurrent (threadDelay, forkIO)
import Miso.Subscription.Keyboard (keyboardSub)
import qualified Data.Set as Set

{-
newtype View2 action = View2 {
  runView :: (action -> IO ()) -> IO Int
}
instance Functor View2 where
  fmap ab (View2 faf) = View2 $ \bf ->
    faf (\a -> bf (ab a))
-}

type Text = MisoString

data TimerState = Buzzing | Counting | Idle
                deriving (Show, Eq)

-- | Type synonym for an application model
data Model = Model {
    _seconds :: !Int
  , _setSeconds :: Int
  , _state :: TimerState
  } deriving (Show, Eq)
makeLenses ''Model

-- | Sum type for application events
data Action
  = MinusOne
  | PlusOne
  | Buzz
  | StopBuzz
  | Start
  | CountDown
  | Tick
  | SpaceBar
  | NoOp
  deriving (Show, Eq)

initModel :: Int -> Model
initModel n = Model 0 n Idle

app :: Int -> App Model Action
app startTime = App  {
      initialAction = NoOp
    , model  = Model 0 startTime Idle
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = [ batchSubs [ spaceSub
                           , tickSub 5000000 NoOp ]
               ]
    }

-- | Entry point for a miso application
main :: IO ()
main = startApp $ app 10

batchSubs :: [Sub action model] -> Sub action model
batchSubs = foldr combine (\_ _ -> return ())
  where
    combine sub1 sub2 = \getm sink -> sub1 getm sink >> sub2 getm sink

tickSub :: Int -> action -> Sub action Model
tickSub millis action getm sink =
  void . forkIO . forever $ do
    threadDelay millis
    m <- getm
    print $ m ^. state
    sink action

spaceSub :: Sub Action Model
spaceSub = keyboardSub $ \keys -> if Set.member 32 keys
                                  then SpaceBar
                                  else NoOp

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel MinusOne m = return $ m & setSeconds %~ (subtract 1)
updateModel PlusOne m = return $ m & setSeconds %~ (+1)
updateModel Start m = (m & state .~ Counting
                         & seconds .~ (m ^. setSeconds))
                      <# return CountDown
updateModel CountDown m = case m ^. state of
  Counting -> if m ^. seconds <= 0
                then m <# return Buzz
                else (m & seconds %~ (subtract 1)) <# do
                  _ <- threadDelay 1000000
                  return CountDown
  _ -> return m
updateModel StopBuzz m = return $ m & state .~ Idle
updateModel Buzz m = return $ m & state .~ Buzzing
updateModel Tick m = m <# do
  putStrLn "tick"
  return NoOp
updateModel SpaceBar m = m <# (return $ case m ^. state of
  Idle -> Start
  _ -> StopBuzz )
updateModel NoOp m = return m


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = div_ [] $ concat [
    [ span_ [] [ case m ^. state of
               Buzzing -> text "BUZZZZZZ"
               Counting -> text . ms . show $ m ^. seconds
               Idle -> text . ms . show $ m ^. setSeconds ] ]
  , [ button_ [ onClick PlusOne ] [ text "+" ] ]
  , [ button_ [ onClick MinusOne ] [ text "-" ] ]
  , [ case m ^. state of
      Buzzing -> button_ [ onClick StopBuzz ] [ text "Stop the BuzzZZZ" ]
      Counting -> button_ [ onClick StopBuzz ] [ text "Reset" ]
      Idle -> button_ [ onClick Start ] [ text "Start Countdown" ]
    ]
  ]
