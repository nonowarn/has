{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances,UndecidableInstances #-}

module Data.Has
  ( (:*:)(..)
  , Has(..)
  , upd
  ) where

import Data.Maybe

infixr 5 :*:

data a :*: b = a :*: b
    deriving (Eq,Ord,Show,Read,Bounded)

upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s

data TyTrue = TyTrue
data TyFalse = TyFalse

instance Show TyTrue where show _ = "TyTrue"
instance Show TyFalse where show _ = "TyFalse"

class Has e s where
    inj :: e -> s -> s
    prj :: s -> e

class MayHas e s where
    inj' :: e -> s -> Maybe s
    prj' :: s -> Maybe e

class Contains e s bool | e s -> bool where
    contains :: e -> s -> bool

instance (MayHas e s, Contains e s TyTrue) => Has e s where
    inj e s = fromJust (inj' e s)
    prj s   = fromJust (prj' s)

instance MayHas e f where
    inj' _ _ = Nothing
    prj' _   = Nothing
instance MayHas e e where
    inj' e _ = Just e
    prj' e   = Just e
instance (MayHas e h,MayHas e t) => MayHas e (h :*: t) where
    inj' e ~(h:*:t) = maybe (fmap (h:*:) (inj' e t)) (Just . (:*:t)) (inj' e h)
    prj'   ~(h:*:t) = maybe (prj' t)   Just (prj' h)

instance (b ~ TyTrue) => Contains e e b where
    contains _ _ = TyTrue
instance (b ~ TyFalse) => Contains e f b where
    contains _ _ = TyFalse
instance (TyOr x y b, Contains e h x, Contains e t y) => Contains e (h :*: t) b where
    contains e ~(h:*:t) = contains e h `tyor` contains e t

class TyOr a b r | a b -> r where
    tyor :: a -> b -> r

instance TyOr TyTrue b TyTrue where
    tyor _ _ = TyTrue
instance TyOr TyFalse b b where
    tyor _ b = b
