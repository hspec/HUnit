#!/usr/bin/env runhaskell
module Main (main) where

import Data.List (isSuffixOf)
import Distribution.PackageDescription
import Distribution.Simple
import System.FilePath
import System.Process

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks {runTests = _runTests, instHook = _instHook})
    where
        -- Run all executables with names that end in -tests
        _runTests _ _ pd _ = do
            let exeNames = ["dist" </> "build" </> fp </> fp | fp <- map exeName (executables pd)]
            sequence [_runTest e | e <- exeNames, isSuffixOf "-tests" e]
            return ()
        _runTest fp = do
            ph <- runCommand fp
            waitForProcess ph
        
        -- Only install executables that don't end in -tests
        _instHook pd lbi uhs ifs = do
            let execs = filter (\e -> not $ isSuffixOf "-tests" (exeName e)) (executables pd)
            (instHook simpleUserHooks) (pd {executables = execs}) lbi uhs ifs 
        