{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
--       , testGroup "Corner Cases"  test_corner_cases
--       , testGroup "Newtypes"      test_newtypes
--       , testGroup "Labelled Values" test_labelled_values
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

-- test_corner_cases =
--     [ testGroup "If there are same types in a type list"
--       [ eq "left-most data wins in injection"
--                (P (-1) :*: P 2 :*: P 3)
--                (inj (P (-1)) (P 1 :*: P 2 :*: P 3))
--       , eq "left-most data wins in projection also"
--                (P 1)
--                (prj (P 1 :*: P 2 :*: P 3))
--       , eq "even they are nested complexly"
--                (P 0)
--                (prj (P 0 :*: (P 1 :*: (P 2 :*: P 3) :*: (P 4 :*: P 5)) :*: P 6))
--       , eq "even the type does not occur outer-most"
--                (P 2)
--                (prj (Q 0 :*: (R 1 :*: (P 2 :*: R 3) :*: P 4) :*: R 5 :*: Q 6))
--       ]
--     ]

-- newtype NT = NT (P :*: Q)
--     deriving (Show,Eq,Has P,Has Q)
-- newtype NT' = NT' (P :*: NT)
--     deriving (Show,Eq,Has P,Has Q)
-- newtype NT'' = NT'' (P :*: NT')
--     deriving (Show,Eq,Has P,Has Q)

-- Newtypes: If we place Q to be the right side of NT' as follows,
--
-- > newtype NT'' = NT'' (NT' :*: Q)
-- >   deriving (Show,Eq,Has P,Has Q)
--
-- It gives us a compile error, due to unable NT'' to derive (Has Q).
-- This is because of limition of instance declaration. If I can write
-- this instance in Data.Has, We are happy.
--
-- > instance Has e h => Has e (h :*: t)
--
-- This instance is Multiple definition to this instance.
--
-- > instance Has e t => Has e (h :*: t)
--
-- This issue make my head ache. So I don't know how to get rid of
-- this, I ignore and recomend users to cons'ing a type always from
-- left.

-- test_newtypes =
--     [ testGroup "can derive Has class with GND"
--       [ eq "it works in injection"
--                (NT $ P 4 :*: Q 2)
--                (inj (P 4) (NT $ P 2 :*: Q 2))
--       , eq "it works in projection"
--                (Q 2) (prj (NT $ P 2 :*: Q 2))
--       ]
--     , testGroup "can wrap another newtype and derive instances"
--       [ testGroup "outer-left-most type still wins"
--         [ eq "in injection"
--              (NT' $ P 10 :*: NT (P 0 :*: Q 0))
--              (inj (P 10) (NT' $ P 0 :*: NT (P 0 :*: Q 0)))
--         , eq "in projection"
--              (P 10) (prj (NT' $ P 10 :*: NT (P 0 :*: Q 0)))
--         , eq "more nestings"
--              (P 77) (prj (NT'' $ P 77 :*: NT' (P 10 :*: NT (P 0 :*: Q 0))))
--         ]
--       ]
--     ]

-- data X = X; data Y = Y; data Z = Z;

-- test_labelled_values =
--     [ eq "inject a value by a label"
--          (X .> "foo" :*: Y .> "bar" :*: Z .> "baz")
--          (injl X "foo" (X .> "boo" :*: Y .> "bar" :*: Z .> "baz"))
--     , eq "project a value by a label"
--          "bar"
--          (prjl Y (X .> "boo" :*: Y .> "bar" :*: Z .> "baz"))
--     , eq "update a value by a label"
--          (X .> "foofoo" :*: Y .> "bar" :*: Z .> "baz")
--          (updl X (++"foo") (X .> "foo" :*: Y .> "bar" :*: Z .> "baz"))
--     ]
