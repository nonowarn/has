{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides some pliant data types and functions.

module Data.Has
  (
  -- * Core
    (&), (:&:), row
  , Has(..), Row
  , upd

  -- * Working with labelled values
  , Labelled(), (:>), (.>)
  , injl, prjl, updl
  ) where

import Control.Applicative
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Data.Monoid (Monoid (..))

import Data.Has.Engine

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
class (Has (Labelled lab e) s) => HasLabelled lab e s | lab s -> e where
    -- | Injects a labelled value
    injl :: lab -> e -> s -> s
    -- | Projects a labelled value
    prjl :: lab -> s -> e

instance (Has (Labelled lab e) s) => HasLabelled lab e s where
    injl lab e s = inj (label lab e) s
    prjl lab s   = unlabel lab (prj s)

-- | Updates a labelled value
updl :: (HasLabelled lab b a)
     => lab -> (b -> b) -> (a -> a)
updl lab f a = let b = prjl lab a in injl lab (f b) a

-- And Instances
instance (Monoid a) => Monoid (Labelled lab a) where
    mempty = Label mempty
    mappend a b = Label (unLabelled a `mappend` unLabelled b)

instance (Arbitrary a) => Arbitrary (Labelled lab a) where
    arbitrary = Label <$> arbitrary

instance (CoArbitrary a) => CoArbitrary (Labelled lab a) where
    coarbitrary = coarbitrary . unLabelled
