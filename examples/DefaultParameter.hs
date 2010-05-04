{-# OPTIONS_GHC -fglasgow-exts #-}

import Control.Monad
import System.Environment.UTF8

import Data.Has

data Name = Name; type instance TypeOf Name = String
data Greeting = Greeting; type instance TypeOf Greeting = String
data NumGreet = NumGreet; type instance TypeOf NumGreet = Int
data WithNewLine = WithNewLine; type instance TypeOf WithNewLine = Bool

type GreetOpt = FieldOf Name
              :&: FieldOf Greeting
              :&: FieldOf NumGreet
              :&: FieldOf WithNewLine

greet :: GreetOpt -> IO ()
greet gopt = replicateM_
               (NumGreet ^. gopt)
               ((if WithNewLine ^. gopt then putStrLn else putStr)
                ((Greeting ^. gopt) ++ ", " ++ (Name ^. gopt) ++ "."))

main = do
    gopt <- fmap parse getArgs
    greet gopt
  where
    parse :: [String] -> GreetOpt
    parse ("-n":params) = WithNewLine ^= False $ parse params
    parse (name:greeting:number:params) | ((numGreet,_):_) <- reads number =
       Name ^= name
     $ Greeting ^= greeting
     $ NumGreet ^= numGreet $ parse params
    parse _ = fieldOf "an anonymous user"
            & fieldOf "Hello"
            & fieldOf 1
            & fieldOf True
