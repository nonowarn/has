{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has
import Data.Has.Engine

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
       , testGroup "Corner Cases"  test_corner_cases
       , testGroup "Labelled Values" test_labelled_values
       ]

eq test_name expected actual =
    testCase test_name $ assertEqual test_name expected actual

newtype P = P Int deriving (Eq,Show)
newtype Q = Q Int deriving (Eq,Show)
newtype R = R Int deriving (Eq,Show)

pqr :: Int -> Int -> Int -> Field P :&: Field Q :&: Field R
pqr p q r = field (P p) & field (Q q) & field (R r)

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

    , let intBool = field (1::Int) & field True
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

data X = X; data Y = Y; data Z = Z;
type instance TypeOf X = String
type instance TypeOf Y = String
type instance TypeOf Z = String

type C = FieldOf X :&: FieldOf Y :&: FieldOf Z
mkC :: String -> String -> String -> C
mkC x y z = fieldOf x & fieldOf y & fieldOf z

test_labelled_values =
    [ eq "inject a value by a label"
         (mkC "foo" "bar" "baz")
         (X ^= "foo" $ mkC "boo" "bar" "baz")
    , eq "project a value by a label"
         "bar"
         (Y ^. mkC "boo" "bar" "baz")
    , eq "update a value by a label"
         (X .> "foofoo" & Y .> "bar" & Z .> "baz")
         (X ^: (++"foo") $ mkC "foo" "bar" "baz")
    ]
