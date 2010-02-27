{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.Has
  ( (:*:)(..)
  , Has(..)
  , Tag
  ) where

infixr 5 :*:

data a :*: b = a :*: b
    deriving (Eq,Ord,Show,Read,Bounded)

data family Tag e

class Has e s where
    inj :: Tag e -> s -> e -> s
    prj :: Tag e -> s -> e

instance Has e (e :*: r) where
    inj _ (_ :*: r) e = e :*: r
    prj _ (e :*: _)   = e

instance Has e r => Has e (b :*: r) where
    inj t (b :*: r) e = b :*: inj t r e
    prj t (b :*: r)   = prj t r

-- This instance is needed for last types of type lists such as T in
-- (T1 :*: T2 :*: T3 :*: T)
instance Has e e where
    inj _ _ e = e
    prj _ e   = e
