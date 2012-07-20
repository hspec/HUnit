-- HUnitTests.hs
--
-- This file is an entry point for running all of the tests.

module Main (main) where

import System.Exit

import Test.HUnit
import HUnitTestBase
import HUnitTestExtended
import HUnitTestOptimize
import TerminalTest

main :: IO ()
main = do
    counts2 <- runTestTT (test [
            baseTests, 
            extendedTests,
            undefinedSwallowsTests,
            terminalTests
            ])
    oPassed <- optimizationTests
    if (errors counts2 + failures counts2 == 0 && oPassed) 
        then exitSuccess
        else exitFailure