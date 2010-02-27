{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
       , testGroup "Corner Cases"  test_corner_cases
       ]

eq test_name expected actual =
    testCase test_name $ assertEqual test_name expected actual

newtype P = P Int deriving (Eq,Show)
newtype Q = Q Int deriving (Eq,Show)
newtype R = R Int deriving (Eq,Show)

data instance Tag P = OfP
data instance Tag Q = OfQ
data instance Tag R = OfR

test_typical_usage =
    [ eq "Project by Tag" (Q 2) (prj OfQ (P 1 :*: Q 2 :*: R 3))
    , eq "Inject by Tag"  (P 1 :*: Q 10 :*: R 3)
                          (inj OfQ (P 1 :*: Q 2 :*: R 3) (Q 10))

    , eq "Project by Another Tag" (P 1) (prj OfP (P 1 :*: Q 2 :*: R 3))
    , eq "Inject by Another Tag" (P 99 :*: Q 2 :*: R 3)
                                 (inj OfP (P 1 :*: Q 2 :*: R 3) (P 99))
    ]

test_corner_cases =
    [ testGroup "If there are same types in a type list"
      [ eq "left-most data wins in injection"
               (P (-1) :*: P 2 :*: P 3) (inj OfP (P 1 :*: P 2 :*: P 3) (P (-1)))
      , eq "left-most data wins in projection also"
               (P 1) (prj OfP (P 1 :*: P 2 :*: P 3))
      ]
    ]
