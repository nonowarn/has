{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Data.Has

main = defaultMain
       [ testGroup "Typical Usage" test_typical_usage
       , testGroup "Corner Cases"  test_corner_cases
       , testGroup "Newtypes"      test_newtypes
       , testGroup "Poly Values"   test_poly_values
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
    , eq "Update by a Tag" (P (-1) :*: Q 2 :*: R 3)
                           (upd OfP (\(P n) -> P (negate n)) (P 1 :*: Q 2 :*: R 3))
    ]

test_corner_cases =
    [ testGroup "If there are same types in a type list"
      [ eq "outer-left-most data wins in injection"
               (P (-1) :*: P 2 :*: P 3) (inj OfP (P 1 :*: P 2 :*: P 3) (P (-1)))
      , eq "outer-left-most data wins in projection also"
               (P 1) (prj OfP (P 1 :*: P 2 :*: P 3))
      , eq "even they are nested complexly"
               (P 0) (prj OfP (P 0 :*: (P 1 :*: (P 2 :*: P 3) :*: (P 4 :*: P 5)) :*: P 6))
      ]
    ]

newtype NT = NT (P :*: Q)
    deriving (Show,Eq,Has P,Has Q)
newtype NT' = NT' (P :*: NT)
    deriving (Show,Eq,Has P,Has Q)
newtype NT'' = NT'' (P :*: NT')
    deriving (Show,Eq,Has P,Has Q)

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

test_newtypes =
    [ testGroup "can derive Has class with GND"
      [ eq "it works in injection"
               (NT $ P 4 :*: Q 2)
               (inj OfP (NT $ P 2 :*: Q 2) (P 4))
      , eq "it works in projection"
               (Q 2) (prj OfQ (NT $ P 2 :*: Q 2))
      ]
    , testGroup "can wrap another newtype and derive instances"
      [ testGroup "outer-left-most type still wins"
        [ eq "in injection"
             (NT' $ P 10 :*: NT (P 0 :*: Q 0))
             (inj OfP (NT' $ P 0 :*: NT (P 0 :*: Q 0)) (P 10))
        , eq "in projection"
             (P 10) (prj OfP (NT' $ P 10 :*: NT (P 0 :*: Q 0)))
        , eq "more nestings"
             (P 77) (prj OfP (NT'' $ P 77 :*: NT' (P 10 :*: NT (P 0 :*: Q 0))))
        ]
      ]
    ]

-- Poly Values: Type signiture is important here...
--
-- If we write
--
-- > intBool = 1 :*: True
-- > v = poly intBool
--
-- GHC infers v's type should be (Has e Bool, Num e) => e. This might
-- be a bug of the compiler. But if we have type signitures as
-- follows, everything works fine.
--
-- > intBool :: Int :*: Bool
-- > v :: Int

test_poly_values =
  let
    intBool :: Int :*: Bool
    intBool = 1 :*: True
  in
    [ eq "selector is determined at compile time"
         (2::Int) (if poly intBool then poly intBool + 1 else 0)
    ]
