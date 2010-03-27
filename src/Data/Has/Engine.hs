{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE OverlappingInstances,UndecidableInstances #-}

module Data.Has.Engine where

import Control.Applicative
import Data.Monoid (Monoid (..))
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..))

-- | Represents Type level list.
data a :*: b = a :*: b
    deriving (Eq,Ord,Show,Read,Bounded)

infixr 5 :*:

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
    mempty = mempty :*: mempty
    mappend ~(a :*: b) ~(a' :*: b') = mappend a a' :*: mappend b b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
    arbitrary = liftA2 (:*:) arbitrary arbitrary

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a :*: b) where
    coarbitrary ~(a :*: b) = coarbitrary a . coarbitrary b


-- | Represents Type level boolean True
data TyTrue = TyTrue

-- | Represents Type level boolean False
data TyFalse = TyFalse

instance Show TyTrue where show _ = "TyTrue"
instance Show TyFalse where show _ = "TyFalse"

-- | Class for trying injection and projection of values.
--   If there are no matching types, it fails with returning Nothing.
--   If there are many matching types, left-most type will win.
class MayHave e s where
    inj' :: e -> s -> Maybe s
    prj' :: s -> Maybe e

-- | Class which predicts if type list @s@ contains @e@.
--   if @s@ contains @e@, @bool@ should be @TyTrue@.
--   if @s@ doesn't contains @e@, @bool@ should be @TyFalse@.
class Contains e s bool | e s -> bool where
    contains :: e -> s -> bool

-- | Class which calculates type level disjunction.
class TyOr a b r | a b -> r where
    tyor :: a -> b -> r

{-
  And implementaions.
  Methods in classes are not often used, but make debugging easier.
 -}

instance MayHave e f where
    inj' _ _ = Nothing
    prj' _   = Nothing
instance MayHave e e where
    inj' e _ = Just e
    prj' e   = Just e
instance (MayHave e h,MayHave e t) => MayHave e (h :*: t) where
    inj' e ~(h:*:t) = maybe (fmap (h:*:) (inj' e t)) (Just . (:*:t)) (inj' e h)
    prj'   ~(h:*:t) = maybe (prj' t)   Just (prj' h)

instance (b ~ TyTrue) => Contains e e b where
    contains _ _ = TyTrue
instance (b ~ TyFalse) => Contains e f b where
    contains _ _ = TyFalse
instance (TyOr x y b, Contains e h x, Contains e t y) => Contains e (h :*: t) b where
    contains e ~(h:*:t) = contains e h `tyor` contains e t

instance TyOr TyTrue b TyTrue where
    tyor _ _ = TyTrue
instance TyOr TyFalse b b where
    tyor _ b = b
