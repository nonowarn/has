{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Data.Has
-- Copyright   :  HASHIMOTO, Yusaku 2010
-- License     :  BSD3
--
-- Maintainer  :  nonowarn@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Entiry based records.

module Data.Has
  (
  -- * Core
    (&), (:&:), row
  , Has(..), Row
  , upd

  -- * Working with labelled values
  , Labelled(), (:>), (.>)
  , Knows(..), updl

  -- ** And aliases
  , (^=), (.^)

  -- * Make parsing error messages easier
  , (:::), TyNil()
  ) where

import Control.Applicative
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Data.Monoid (Monoid (..))

import Data.Has.Engine
import Data.Has.TypeList ((:::), TyNil)

-- | Updates a value @e@ in @s@, using given function @e -> e@.
upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s


-- Labelled Values

-- | Represents labelled value.
newtype Labelled lab a = Label { unLabelled :: a }
    deriving (Eq,Ord,Show,Read,Bounded)

-- | Represents labelled row.
type lab :> a = Row (Labelled lab a)
infix 6 :>

-- | Attaches a label.
label :: lab -> a -> Labelled lab a
label _ a = Label a

-- | Detaches a label.
unlabel :: lab -> Labelled lab a -> a
unlabel _ = unLabelled

-- | Makes a labelled row.
(.>) :: lab -> a -> lab :> a
(.>) = (row .) . label

infix 6 .>

-- | Injects and Projects a labelled values into records.
class (Has (Labelled lab e) s) => Knows lab e s | lab s -> e where
    -- | Injects a labelled value
    injl :: lab -> e -> s -> s
    -- | Projects a labelled value
    prjl :: lab -> s -> e

instance (Has (Labelled lab e) s) => Knows lab e s where
    injl lab e s = inj (label lab e) s
    prjl lab s   = unlabel lab (prj s)

-- | Updates a labelled value
updl :: (Knows lab b a)
     => lab -> (b -> b) -> (a -> a)
updl lab f a = let b = prjl lab a in injl lab (f b) a

-- | Opeartor version of 'injl'
(^=) :: (Knows lab e s) => lab -> e -> s -> s
(^=) = injl
infix 6 ^=

-- | Operator version of 'prjl'
(.^) :: (Knows lab e s) => lab -> s -> e
(.^) = prjl
infix 4 .^

-- And misc instances
instance (Monoid a) => Monoid (Labelled lab a) where
    mempty = Label mempty
    mappend a b = Label (unLabelled a `mappend` unLabelled b)

instance (Arbitrary a) => Arbitrary (Labelled lab a) where
    arbitrary = Label <$> arbitrary

instance (CoArbitrary a) => CoArbitrary (Labelled lab a) where
    coarbitrary = coarbitrary . unLabelled
