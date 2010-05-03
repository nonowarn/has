{-# OPTIONS_GHC -fglasgow-exts #-}

import Control.Monad
import System.Environment.UTF8

import Data.Has

data Name = Name; type instance TypeOf Name = String
data Greeting = Greeting; type instance TypeOf Greeting = String
data NumGreet = NumGreet; type instance TypeOf NumGreet = Int
data WithNewLine = WithNewLine; type instance TypeOf WithNewLine = Bool

type GreetOpt = RowOf Name
              :&: RowOf Greeting
              :&: RowOf NumGreet
              :&: RowOf WithNewLine

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
    parse _ = rowOf "an anonymous user"
            & rowOf "Hello"
            & rowOf 1
            & rowOf True
