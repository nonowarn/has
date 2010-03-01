{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.Has
  ( (:*:)(..)
  , Has(..)
  , upd
  ) where

infixr 5 :*:

data a :*: b = a :*: b
    deriving (Eq,Ord,Show,Read,Bounded)

class Has e s where
    inj :: e -> s -> s
    prj :: s -> e

instance Has e (e :*: r) where
    inj e ~(_ :*: r) = e :*: r
    prj   ~(e :*: _) = e

instance Has e r => Has e (b :*: r) where
    inj e ~(b :*: r) = b :*: inj e r
    prj   ~(b :*: r) = prj r

-- This instance is needed for last types of type lists such as T in
-- (T1 :*: T2 :*: T3 :*: T)
instance Has e e where
    inj e _ = e
    prj e   = e

upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s
