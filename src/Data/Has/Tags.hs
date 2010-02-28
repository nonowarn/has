{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Has.Tags where

import Data.Has

infixr 6 :->

data instance Tag ()           = OfUnit
data instance Tag Bool         = OfBool
data instance Tag Char         = OfChar
data instance Tag (Maybe a)    = OfMaybe (Tag a)
data instance Tag (Either a b) = OfEither (Tag a) (Tag b)
data instance Tag (IO a)       = OfIO (Tag a)
data instance Tag Ordering     = OfOrdering
data instance Tag Int          = OfInt
data instance Tag Integer      = OfInteger
data instance Tag Float        = OfFloat
data instance Tag Double       = OfDouble
data instance Tag [a]          = OfList (Tag a)
data instance Tag (a,b)        = OfTuple (Tag a) (Tag b)
data instance Tag (a,b,c)      = OfTriple (Tag a) (Tag b) (Tag c)
data instance Tag (a -> b)     = Tag a :-> Tag b
