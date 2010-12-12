{-# LANGUAGE TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}

module Main where

import Data.Has

-- This example is based on chris' blog post: <http://chrisdone.com/posts/2010-11-22-duck-typing-in-haskell.html>.
-- If you don't hate template haskell, you can write below ugly things with simple notation with has-th package by chris.
--
-- > field "flap"  [t|IO ()|]
-- > field "quack" [t|String|]
-- > field "swim"  [t|IO ()|]
-- > data Food = Bread | Pasta | Duck deriving Show
-- > field "eat"   [t|Food -> Either String String|]
-- > field "name"  [t|String|]
-- > field "age"   [t|Integer|]
--
-- But I think has package should not depend has-th, so I write bare entities in this example.

data Flap = Flap; type instance TypeOf Flap = IO ()
data Quack = Quack; type instance TypeOf Quack = String
data Swim = Swim; type instance TypeOf Swim = IO ()
data Food = Bread | Pasta | Duck deriving Show
data Eat = Eat; type instance TypeOf Eat = Food -> Either String String
data Name = Name; type instance TypeOf Name = String
data Age = Age; type instance TypeOf Age = Integer
flap = Flap
quack = Quack
swim = Swim
eat = Eat
name = Name
age = Age

donald = flap  ^- putStrLn "*Flap flap flap*"
       & quack ^- "QUACK!"
       & swim  ^- putStrLn "*Ducky paddle*"
       & eat   ^- nom
       & name  ^- "Donald Duck"
  where nom Bread = Right "Nom nom nom!"
        nom Pasta = Right "Ew, give me something bweady."
        nom Duck  = Left  "Are you cwazy!?"

chris = flap  ^- putStrLn "I'm flapping my arms!"
      & quack ^- "Erm, quack?"
      & swim  ^- putStrLn "I'm doing the butterfly because it's efficient ..."
      & age   ^- 67
      & eat   ^- nom
      & name  ^- "Chris Done"
  where nom Bread = Left  "Bread is alright, but no thanks."
        nom Pasta = Right "Pasta is okay. Got anything to go with it?"
        nom Duck  = Right "Om nom nom."

fly :: (Has Flap duck) => duck -> IO ()
fly duck = do go; go; go where go = flap ^. duck

playInPond :: (Has Swim duck, Has Flap duck, Has Eat duck, Has Name duck)
           => duck -> Food -> IO ()
playInPond duck food = do
  putStrLn $ (Name ^. duck) ++ " is swimming happily."
  Swim ^. duck
  putStrLn $ "You give them some " ++ show food ++ " to eat."
  case (eat ^. duck) food of
    Left dnw  -> do putStrLn dnw; fly duck
    Right nom -> putStrLn nom
  Swim ^. duck

main :: IO ()
main = do
    fly donald
    fly chris
    playInPond donald Duck
    playInPond chris Bread
