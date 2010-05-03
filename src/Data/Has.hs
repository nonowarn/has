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
  , (^=), (^.), (^:)

  -- ** Defining labels
  , TypeOf, RowOf, rowOf

  -- * Make parsing error messages easier
  , (:::), TyNil()
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

type family TypeOf a

type family RowOf a
type instance RowOf a = a :> TypeOf a

rowOf :: TypeOf a -> RowOf a
rowOf a = undefined .> a

class (Knows lab (TypeOf lab) s) => Has lab s
instance (Knows lab (TypeOf lab) s) => Has lab s

-- | Opeartor version of 'injl'
(^=) :: (Knows lab (TypeOf lab) s) => lab -> TypeOf lab -> s -> s
(^=) = injl
infix 6 ^=

-- | Operator version of 'prjl'
(^.) :: (Knows lab (TypeOf lab) s) => lab -> s -> TypeOf lab
(^.) = prjl
infix 4 ^.

-- | Operator version of 'updl'
(^:) :: (Knows lab (TypeOf lab) s)=> lab -> (TypeOf lab -> TypeOf lab) -> (s -> s)
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
