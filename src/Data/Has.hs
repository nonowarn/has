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
  -- * Has class
    Has

  -- * Rows in records
  , Row
  , (&), (:&:), row

  -- * Update and Lookup values from records
  , (^=), (^.), (^:)
  , Knows(..), updl

  -- * Labelled values
  , Labelled(), (:>), (.>)

  -- ** Defining labels
  , TypeOf, RowOf, rowOf

  -- * Make parsing error messages easier
  , (:::)(), TyNil(), Contains()
  ) where

import Control.Applicative
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Data.Monoid (Monoid (..))

import Data.Has.Engine
import Data.Has.TypeList ((:::), TyNil)

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
class (Contains (Labelled lab e) s) => Knows lab e s | lab s -> e where
    -- | Injects a labelled value
    injl :: lab -> e -> s -> s
    -- | Projects a labelled value
    prjl :: lab -> s -> e

instance (Contains (Labelled lab e) s) => Knows lab e s where
    injl lab e s = inj (label lab e) s
    prjl lab s   = unlabel lab (prj s)

-- | Updates a labelled value
updl :: (Knows lab b a)
     => lab -> (b -> b) -> (a -> a)
updl lab f a = let b = prjl lab a in injl lab (f b) a

-- | TypeOf @a@ should indicate a type labelled by @a@
type family TypeOf a

-- | > RowOf a == a :> TypeOf a
type family RowOf a
type instance RowOf a = a :> TypeOf a

-- | Creates a row labelled by @a@
rowOf :: TypeOf a -> RowOf a
rowOf a = undefined .> a

-- | Same as @Knows lab (TypeOf lab) s@, Useful on writing type
--   signitures.
--
--   Holds @e == (lab .^ (lab ^= e $ s))@
--   where @lab :: lab; e :: TypeOf lab; s :: s@
class (Knows lab (TypeOf lab) s) => Has lab s
instance (Knows lab (TypeOf lab) s) => Has lab s

-- | Strict version of 'injl'
(^=) :: (Knows lab (TypeOf lab) s)
     => lab -> TypeOf lab -> s -> s
(^=) = injl
infix 6 ^=

-- | Strict version of 'prjl'
(^.) :: (Knows lab (TypeOf lab) s)
     => lab -> s -> TypeOf lab
(^.) = prjl
infix 4 ^.

-- | Strict version of 'updl'
(^:) :: (Knows lab (TypeOf lab) s)
     => lab -> (TypeOf lab -> TypeOf lab) -> (s -> s)
(^:) = updl
infixr 5 ^:

-- And misc instances
instance (Monoid a) => Monoid (Labelled lab a) where
    mempty = Label mempty
    mappend a b = Label (unLabelled a `mappend` unLabelled b)

instance (Arbitrary a) => Arbitrary (Labelled lab a) where
    arbitrary = Label <$> arbitrary

instance (CoArbitrary a) => CoArbitrary (Labelled lab a) where
    coarbitrary = coarbitrary . unLabelled
