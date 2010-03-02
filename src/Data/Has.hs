{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances,UndecidableInstances #-}

module Data.Has
  ( (:*:)(..)
  , Has(..)
  , upd
  ) where

import Data.Maybe

import Data.Has.Engine

class Has e s where
    inj :: e -> s -> s
    prj :: s -> e

upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s

instance (MayHas e s, Contains e s TyTrue) => Has e s where
    inj e s = fromJust (inj' e s)
    prj s   = fromJust (prj' s)
