{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
       , testGroup "Corner Cases"  test_corner_cases
       , testGroup "Newtypes"      test_newtypes
       , testGroup "Labelled Values" test_labelled_values
       ]

eq test_name expected actual =
    testCase test_name $ assertEqual test_name expected actual

newtype P = P Int deriving (Eq,Show)
newtype Q = Q Int deriving (Eq,Show)
newtype R = R Int deriving (Eq,Show)

pqr :: Int -> Int -> Int -> Row P :&: Row Q :&: Row R
pqr p q r = row (P p) & row (Q q) & row (R r)

test_typical_usage =
    [ eq "Project by Type" (Q 2) (prj (pqr 1 2 3))
    , eq "Inject by Type"
             (pqr 1 10 3)
             (inj (Q 10) (pqr 1 2 3))

    , eq "Project by Another Type" (P 1) (prj (pqr 1 2 3))
    , eq "Inject by Another Type"
             (pqr 99 2 3)
             (inj (P 99) (pqr 1 2 3))

    , eq "Update by a Type"
             (pqr (-1) 2 3)
             (upd (\(P n) -> P (negate n)) (pqr 1 2 3))

    , eq "Build data by inj by inj"
             (pqr 1 2 3)
             (inj (P 1) . inj (R 3) . inj (Q 2) $ undefined)

    , let intBool = row (1::Int) & row True
      in eq "prj selects a value from record with type inference"
             (2::Int) (if prj intBool then prj intBool + 1 else -1)
    ]

data L = L; data M = M; data N = N

test_corner_cases =
    [ testGroup "If there are same types in a type list"
      [ eq "left-most data wins in injection"
               (L .> 'a' & L .> 'b' & L .> 'c')
               (injl L 'a' (L .> 'x' & L .> 'b' & L .> 'c'))
      , eq "left-most data wins in projection also"
               'a'
               (prjl L (L .> 'a' & L .> 'b' & L .> 'c'))
      , eq "even they are nested complexly"
               'a'
               (prjl L
                ((L .> 'a' & L .> 'b') & (L .> 'c' & L .> 'd' & L .> 'e') & L .> 'f'))
      , eq "even the type does not occur outer-most"
               'c'
               (prjl L (M .> 'a' & (N .> 'b' & (L .> 'c' & M .> 'd') & L .> 'e')))
      ]
    ]

newtype NT = NT (Row P :&: Row Q)
    deriving (Show,Eq,Has P,Has Q)
newtype NT' = NT' (Row P :&: NT)
    deriving (Show,Eq,Has P,Has Q) -- This is OK
newtype NT'' = NT'' (Row P :&: NT')
    deriving (Show,Eq,Has P,Has Q) -- This is also OK

-- But following data declaration can't derive Has.
--
-- > newtype NT3 = NT3 (NT :&: Row P)
-- >     deriving (Show,Eq,Has P)
-- > newtype NT4 = NT4 (NT :&: NT)
-- >     deriving (Show,Eq,Has P)
--
-- Because (:&:) actually is an append operator on type lists. It
-- recurses on its left argument. So if left argument is not type
-- list, the Compiler can't what actual type of it, complains "no
-- instances for this" (at least ghc does. I think it should be "don't
-- know how to append a data which isn't a type list onto a type
-- list")

test_newtypes =
    [ testGroup "can derive Has class with GND"
      [ eq "it works in injection"
               (NT $ p 4 & q 2)
               (inj (P 4) (NT $ p 2 & q 2))
      , eq "it works in projection"
               (Q 2) (prj (NT $ p 2 & q 2))
      ]
    , testGroup "can wrap another newtype and derive instances"
      [ testGroup "outer-left-most type still wins"
        [ eq "in injection"
             (NT' $ p 10 & NT (p 0 & q 0))
             (inj (P 10) (NT' $ p 0 & NT (p 0 & q 0)))
        , eq "in projection"
             (P 10) (prj (NT' $ p 10 & NT (p 0 & q 0)))
        , eq "more nestings"
             (P 77) (prj (NT'' $ p 77 & NT' ((p 10) & NT (p 0 & q 0))))
        ]
      ]
    ]
  where p = row . P; q = row . Q

data X = X; data Y = Y; data Z = Z;

test_labelled_values =
    [ eq "inject a value by a label"
         (X .> "foo" & Y .> "bar" & Z .> "baz")
         (injl X "foo" (X .> "boo" & Y .> "bar" & Z .> "baz"))
    , eq "project a value by a label"
         "bar"
         (prjl Y (X .> "boo" & Y .> "bar" & Z .> "baz"))
    , eq "update a value by a label"
         (X .> "foofoo" & Y .> "bar" & Z .> "baz")
         (updl X (++"foo") (X .> "foo" & Y .> "bar" & Z .> "baz"))
    ]
