{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides type-list functionality

module Data.Has.TypeList where

import Control.Applicative
import Data.Monoid (Monoid (..))
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..))

-- | Cons a type onto type-list.
data a ::: b = a ::: b deriving (Show,Eq,Ord,Read,Bounded)

-- | The empty type-list.
data TyNil = TyNil deriving (Read)

-- | Appends a type-list and another.
class Append a b where
    type a :++: b
    (.++.) :: a -> b -> a :++: b
infixr 5 :++:

-- Implementation of Append

instance Append TyNil b where
    type TyNil :++: b = b
    _ .++. b = b

instance (Append y b) => Append (x ::: y) b where
    type (x ::: y) :++: b = x ::: (y :++: b)
    ~(x ::: y) .++. b = x ::: (y .++. b)

-- Useful Instances

instance (Monoid a, Monoid b) => Monoid (a ::: b) where
    mempty = mempty ::: mempty
    mappend ~(a ::: b) ~(a' ::: b') = mappend a a' ::: mappend b b'
instance (Arbitrary a, Arbitrary b) => Arbitrary (a ::: b) where
    arbitrary = liftA2 (:::) arbitrary arbitrary
instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a ::: b) where
    coarbitrary ~(a ::: b) = coarbitrary a . coarbitrary b

instance Monoid TyNil where mempty = TyNil; mappend = const (const TyNil)
instance Arbitrary TyNil where arbitrary = return TyNil
instance CoArbitrary TyNil where coarbitrary _ = coarbitrary ()

-- And Instances for TyNil should be lazy

const2 :: a -> x -> y -> a
const2 = const . const

instance Eq TyNil where (==) = const2 True
instance Ord TyNil where compare = const2 EQ
instance Bounded TyNil where maxBound = TyNil; minBound = TyNil
instance Show TyNil where show _ = "TyNil"
