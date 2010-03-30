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
  -- , (:>)
  -- , label, unlabel
  -- , (.>), (.<)
  -- , injl, prjl, updl
  ) where

import Data.Has.Engine

-- | Updates a value @e@ in @s@, using given function @e -> e@.
upd :: (Has e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s

-- Labelled values

-- | Represents labelled value
-- newtype (:>) lab a = Lab { unLab :: a }
--     deriving (Eq,Ord,Show,Read,Bounded)

-- instance (Monoid a) => Monoid (lab :> a) where
--     mempty = Lab mempty
--     mappend a b = Lab (unLab a `mappend` unLab b)

-- instance (Arbitrary a) => Arbitrary (lab :> a) where
--     arbitrary = Lab <$> arbitrary

-- instance (CoArbitrary a) => CoArbitrary (lab :> a) where
--     coarbitrary = coarbitrary . unLab

-- -- | attaches a label
-- label :: lab -> a -> lab :> a
-- label _ a = Lab a

-- -- | detaches a label
-- unlabel :: lab -> lab :> a -> a
-- unlabel _ = unLab

-- -- | Operator version of 'label'
-- (.>) :: lab -> a -> lab :> a
-- (.>) = label

-- -- | Operator version of 'unlabel'
-- (.<) :: lab -> lab :> a -> a
-- (.<) = unlabel

-- infix 6 .>
-- infix 6 .<

-- -- | Projects a labelled value
-- prjl :: (Has (lab :> b) a)
--      => lab -> a -> b
-- prjl lab = unlabel lab . prj

-- -- | Injects a labelled value
-- injl :: (Has (lab :> b) a)
--      => lab -> b -> a -> a
-- injl lab b = inj (label lab b)

-- -- | Updates a labelled value
-- updl :: (Has (lab :> b) a)
--      => lab -> (b -> b) -> (a -> a)
-- updl lab f a = let b = prjl lab a in injl lab (f b) a
