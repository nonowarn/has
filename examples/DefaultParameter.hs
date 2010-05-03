{-# OPTIONS_GHC -fglasgow-exts #-}

import Data.Has

import Control.Monad
import System.Environment

data Name = Name
data NumGreet = NumGreet
data WithNewLine = WithNewLine

type GreetOpt =   Name        :> String
              :&: NumGreet    :> Int
              :&: WithNewLine :> Bool

greet :: GreetOpt -> IO ()
greet gopt = replicateM_
               (NumGreet ^. gopt)
               ((if WithNewLine ^. gopt then putStrLn else putStr)
                ("Hello, " ++ (Name ^. gopt) ++ "."))

main = do
    gopt <- fmap parse getArgs
    greet gopt
  where
    parse :: [String] -> GreetOpt
    parse ("-n":params) = WithNewLine ^= False $ parse params
    parse (name:number:params) | ((numGreet,_):_) <- reads number =
       Name ^= name $ NumGreet ^= numGreet $ parse params
    parse _ = Name .> "an anonymous user"
            & NumGreet .> 1
            & WithNewLine .> True
