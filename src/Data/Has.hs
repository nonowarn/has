{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides some pliant data types and functions.

module Data.Has
  ( (:*:)(..)
  , Has(..)
  , upd
  ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid (Monoid (..))
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..))

import Data.Has.Engine

-- | Provides plient function. Holds @e == prj (inj e s)@ for all @s@ and @e@.
class Has e s where
    -- | Injects a value of type @e@ into @s@ if @s@ contains the type @e@.
    inj :: e -> s -> s
    -- | Projects a value of type @e@ out from @s@ if @s@ contains the type @e@.
    prj :: s -> e

-- | Updates a value @e@ in @s@, using given function @e -> e@.
upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s

instance (MayHave e s, Contains e s TyTrue) => Has e s where
    inj e s = fromJust (inj' e s)
    prj s   = fromJust (prj' s)

-- Some orphan instances

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
    mempty = mempty :*: mempty
    mappend ~(a :*: b) ~(a' :*: b') = mappend a a' :*: mappend b b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (a :*: b) where
    arbitrary = liftA2 (:*:) arbitrary arbitrary

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a :*: b) where
    coarbitrary ~(a :*: b) = coarbitrary a . coarbitrary b
