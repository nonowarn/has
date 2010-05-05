{-# LANGUAGE OverlappingInstances #-}

module Data.Has.Engine where

import Data.Has.TypeList

-- | @Field a@ is a type list which contains only one element of
--   @a@. And every field in the records should be this type.
--
--   If you concatenate fields with @(:&:)@ at type-level, @(&)@ at
--   value-level, it becomes a record can be manipulated by functions
--   in this module.
type Field a = a ::: TyNil

-- | Creates a 'Field' of @a@.
field :: a -> Field a
field a = a ::: TyNil

-- | Concatenates between 'Field's or records. Records are
--   concatenated rows. For example, Following expressions are
--   valid.
--
-- > -- Concatenation of rows (i.e. record)
-- > field "string" & field True
--
-- > -- Concatenation of records
-- > (field 'c' & field ()) & (field False & field "string")
--
-- > -- ... And concatenations between a field and a record
-- > field () & (field False & field "string")
-- > (field 'c' & field ()) & field False
(&) :: (Append a b) => a -> b -> a :&: b
(&) = (.++.)
infixr 5 &

-- | Represents concatenated rows or records.
type family a :&: b
type instance a :&: b = a :++: b
infixr 5 :&:

-- | Provides injection and projection into type lists.
--
--   Holds @e == prj (inj e s)@ for all @s@ and @e@.
class Contains e s where
    -- | Injects a value of type @e@ into @s@ if @s@ contains the type @e@.
    inj :: e -> s -> s
    -- | Projects a value of type @e@ out from @s@ if @s@ contains the type @e@.
    prj :: s -> e

instance Contains e (e ::: r) where
    inj e ~(_  ::: r) = e ::: r
    prj   ~(e' ::: _) = e'
instance Contains e r => Contains e (h ::: r) where
    inj e ~(h ::: r) = h ::: inj e r
    prj   ~(_ ::: r) = prj r

-- | Updates a value @e@ in @s@, using given function @e -> e@.
upd :: (Contains e s) => (e -> e) -> s -> s
upd f s = let e = prj s in inj (f e) s
