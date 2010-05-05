(This is a literate haskell style tutorial of the has library, based
on the announce post to haskell-cafe)

I'm pleased to announce the release of my new library, named "has",
written to aim to ease pain at inconvinience of Haskell's built-in
records.

Repository is at GitHub: http://github.com/nonowarn/has
Uploaded on Hackage: http://hackage.haskell.org/package/has

So you can install this by "cabal install has"

With the has, You can reuse accessors over records to write generic
function, combine records with another.

You can use the has in three steps (without counting installation).

1. Write a pragma to enable some extensions at the top of your code,
   import Data.Has module.

> {-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
> import Data.Has

   If you are lazy, you may prefer

< {-# OPTIONS_GHC -fglasgow-exts #-}

2. Define entities. "Entity" is data to index field in records.
   You can define an entity in one line.

> data Foo = Foo; type instance TypeOf Foo = Int

   (I lied) Before semicolon, declares entity. After semicolon,
   specifies the type to which the entity points.

   Define some entities for later examples.

> data Bar = Bar; type instance TypeOf Bar = Double
> data Baz = Baz; type instance TypeOf Baz = String
> data Quux = Quux; type instance TypeOf Quux = Bool

3. Define Records by concatinating fields of entities.

> type MyRecord = FieldOf Foo :&: FieldOf Bar :&: FieldOf Baz

   This is almost same as writing

< data MyRecord = MyRecord { foo :: Int
<                          , bar :: Double
<                          , baz :: String
<                          }

   To construct a value of record, remove colons and replace entities
   in record with values, and uncapitalize some words.

> aRecord :: MyRecord
> aRecord = fieldOf 42 & fieldOf 3.14 & fieldOf "string"

   And you can play with it.

To read/write/modify a value of field in records, you can use
functions with names stealed from data-accessor. But uses value-level
entities instead of accessors.

< Foo ^. aRecord           -- Reading
< Foo ^= 4649 $ aRecord    -- Writing
< Foo ^: (*2) $ aRecord    -- Modifying

If we have another record type contains Foo field, You can still
access the field in the same way.

> type AnotherRecord = FieldOf Bar :&: FieldOf Foo
> anotherRecord :: AnotherRecord
> anotherRecord = fieldOf 2.71 & fieldOf 31

< Foo ^. anotherRecord -- And this also works

Using these functions and Has constraint, You can write generic
functions over records.

> fooIsGreaterThan :: (Has Foo r) => r -> Int -> Bool
> fooIsGreaterThan r x = (Foo ^. r) > x

< aRecord `fooIsGreaterThan` 40       -- evaluated to True
< anotherRecord `fooIsGreaterThan` 40 -- evaluated To False

Even if you defined another record by combining records by (:&:), you
can still access the field, and apply to generic functions.

> type MoreRecord = FieldOf Baz :&: FieldOf Quux
> type CombinedRecord = AnotherRecord :&: MoreRecord
> combinedRecord :: CombinedRecord
> combinedRecord = (fieldOf 1.618 & fieldOf 39) & (fieldOf "sowaka" & fieldOf True)
>                    -- We can omit parentheses
>                    -- (even place parens anyware in record)

< combinedRecord `fooIsGreaterThan` 40 -- This yet works

The Has constraint provides not only genericity but also safety. If
the record doesn't satisfy the constraint, the type checker rejects
it.

> predicateOnRecords :: (Has Foo r, Has Quux r) => r -> Bool
> predicateOnRecords r = fooIsGreaterThan r 30 && (Quux ^. r)

< predicateOnRecords combinedRecord -- This is OK
< predicateOnRecords aRecord        -- This yields compile error

More examples included in package[1]

[1]: http://github.com/nonowarn/has/tree/master/examples/

This library is inspired by HList[2], and interfaces are stealed from
data-accessors[3]. And lenses[4], fclabels[5], and records[6] devote
themselves to similar purposes.

[2]: http://hackage.haskell.org/package/HList
[3]: http://hackage.haskell.org/package/data-accessor
[4]: http://hackage.haskell.org/package/lenses
[5]: http://hackage.haskell.org/package/fclabels
[6]: http://hackage.haskell.org/package/records

Enjoy!

-nwn
