{-# LANGUAGE RankNTypes #-}
module Miso.Component.Lens where

import Miso.Lens (get, set, Lens')

(^.) :: s -> Lens' s a -> a
s ^. l = get l s
infixl 8 ^.

(.~) :: Lens' s a -> a -> s -> s
l .~ a = set l a
infixr 4 .~

(%~) :: Lens' s a -> (a -> a) -> s -> s
l %~ f = \s -> set l (f (get l s)) s
infixr 4 %~

(&) :: a -> (a -> b) -> b
a & f = f a
infixl 1 &

