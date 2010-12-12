-- |
-- Module      :  Data.Has
-- Copyright   :  HASHIMOTO, Yusaku 2010
-- License     :  BSD3
--
-- Maintainer  :  nonowarn@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Entiry based records. To use this module, you have to write
-- LANGUGAGE pragma
--
-- > {-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
--
-- Or OPTIONS_GHC pragma if you are lazy.
--
-- > {-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Has
  (
  -- * Has constraint
    Has

  -- * Fields in Records
  , Field
  , field, fieldOf

  -- * Useful Operators
  , (^=), (^.), (^:), (^-)

  -- * Knows == Generalized version of Has
  , Knows(..), updl

  -- ** Labelled Fields
  , Labelled(), (:>), (.>)

  -- * Defining Entities and Records
  , TypeOf, FieldOf, (&), (:&:)

  -- * Reading error messages easier
  , (:::)(), TyNil(), Contains()
  ) where

import Data.Data
import Control.Applicative
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))
import Data.Monoid (Monoid (..))

import Data.Has.Engine
import Data.Has.TypeList ((:::), TyNil)

-- Labelled Values

-- | Represents labelled value.
newtype Labelled lab a = Label { unLabelled :: a }
    deriving (Eq,Ord,Show,Read,Bounded,Typeable,Data)

-- | Represents labelled field.
type lab :> a = Field (Labelled lab a)
infix 6 :>

-- | Attaches a label.
label :: lab -> a -> Labelled lab a
label _ a = Label a

-- | Detaches a label.
unlabel :: lab -> Labelled lab a -> a
unlabel _ = unLabelled

-- | Makes a labelled field.
(.>) :: lab -> a -> lab :> a
(.>) = (field .) . label

infix 6 .>

-- | Injects and projects a value of @v@  a corresponding field
--   in records @a@ along entity @e@.
--
--   Holds @v == prjl e (injl e v r)@.
class (Contains (Labelled e v) r) => Knows e v r | e r -> v where
    -- | Injects a value @v@ into record @a@ along @e@.
    injl :: e -> v -> r -> r
    -- | Projects a value @v@ into record @a@ along @e@.
    prjl :: e -> r -> v

instance (Contains (Labelled e v) r) => Knows e v r where
    injl e v r = inj (label e v) r
    prjl e r   = unlabel e (prj r)

-- | Updates a value of @v@ in a record @r@ using function of @v -> v@.
updl :: (Knows e v r)
     => e -> (v -> v) -> (r -> r)
updl lab f a = let b = prjl lab a in injl lab (f b) a

-- | @TypeOf a@ should indicate a type labelled by @a@. When defining
--   entities, declare instance of this family. If you want @Foo@
--   entity points to @Int@, you write
--
-- > data Foo = Foo; type instance TypeOf Foo = Int
type family TypeOf a

-- | Field labelled with @a@, and contains @TypeOf a@.
type family FieldOf a
type instance FieldOf a = a :> TypeOf a

-- | Creates a field labelled by @a@
fieldOf :: TypeOf a -> FieldOf a
fieldOf a = undefined .> a

-- | Meaning of this constraint is \"This record @s@ has a field of
--   entity @e@.\" Here, I use the word \"constraint\" for class which
--   is useful on writing type signitures.
--
--   Holds @v == (e .^ (e ^= v $ s))@ where @e :: e; v ::
--   TypeOf e; s :: s@ for all @e@ with @TypeOf e@ and @s@.
--
--   Same as @Knows e (TypeOf e) s@.
class (Knows e (TypeOf e) r) => Has e r
instance (Knows e (TypeOf e) r) => Has e r

-- | Writes field of @e@ in @r@ with @TypeOf e@.
(^=) :: (Knows e (TypeOf e) r)
     => e -> TypeOf e -> r -> r
(^=) = injl
infix 6 ^=

-- | Reads @TypeOf e@ from field of @e@ in @r@.
(^.) :: (Knows e (TypeOf e) r)
     => e -> r -> TypeOf e
(^.) = prjl
infix 4 ^.

-- | Modifies field of @e@ in @r@ with given function @TypeOf e ->
-- | TypeOf e@.
(^:) :: (Knows e (TypeOf e) r)
     => e -> (TypeOf e -> TypeOf e) -> (r -> r)
(^:) = updl
infixr 5 ^:

-- | Creates field of @e@ with given value @TypeOf e@.
--   Stealed from Chris Drone's blog post: <http://chrisdone.com/posts/2010-11-22-duck-typing-in-haskell.html>
(^-) :: e -> TypeOf e -> FieldOf e
(^-) = const fieldOf
infixr 6 ^-

-- And misc instances
instance (Monoid a) => Monoid (Labelled lab a) where
    mempty = Label mempty
    mappend a b = Label (unLabelled a `mappend` unLabelled b)

instance (Arbitrary a) => Arbitrary (Labelled lab a) where
    arbitrary = Label <$> arbitrary

instance (CoArbitrary a) => CoArbitrary (Labelled lab a) where
    coarbitrary = coarbitrary . unLabelled
