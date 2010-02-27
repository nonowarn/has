{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
       , testGroup "Corner Cases"  test_corner_cases
       , testGroup "Newtypes"      test_newtypes
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

newtype NT = NT (P :*: Q)
    deriving (Show,Eq,Has P,Has Q)
newtype NT' = NT' (P :*: NT)
    deriving (Show,Eq,Has P,Has Q)

test_newtypes =
    [ testGroup "can derive Has class with GND"
      [ eq "it works in injection"
               (NT $ P 4 :*: Q 2)
               (inj OfP (NT $ P 2 :*: Q 2) (P 4))
      , eq "it works in projection"
               (Q 2) (prj OfQ (NT $ P 2 :*: Q 2))
      ]
    , testGroup "can wrapp another newtype and derive instances"
      [ testGroup "left most type still wins"
        [ eq "in injection"
             (NT' $ P 10 :*: NT (P 0 :*: Q 0))
             (inj OfP (NT' $ P 0 :*: NT (P 0 :*: Q 0)) (P 10))
        , eq "in projection"
             (P 10) (prj OfP (NT' $ P 10 :*: NT (P 0 :*: Q 0)))
        ]
      ]
    ]
