#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System
import System.FilePath

main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks { runTests = runTests' }

runTests' _ _ _ lbi = system testprog >> return ()
  where
    testprog = (buildDir lbi) </> "test" </> "test"
