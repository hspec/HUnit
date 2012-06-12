-- HUnitTestOptimize.hs
--
-- The purpose of this file is to test whether certain issues occur with optimization.
-- It should be built and run with each level of optimization.  I.e., -O0, -O1, -O2
--
-- With some versions and optimization levels of HUnit and GHC, tests were getting
-- optimized out.  This is a very bad thing and needs to be tested for.

module Main (main) where

import Control.Monad (unless)
import Test.HUnit

-- Used to include the optimization level in the test results
optimizationLevel :: String
#if defined(O0)
optimizationLevel = "-O0"
#elif defined(O1)
optimizationLevel = "-O1"
#elif defined(O2)
optimizationLevel = "-O2"
#else
optimizationLevel = "unknown optimization level"
#endif

-- A test runner that doesn't print the results of the tests; it only tabulates
-- the results of the tests.  In this context, it's used to verify that no
-- tests were optimized away or otherwise lost.
simpleTestRunner :: Test -> IO Counts
simpleTestRunner t = do 
    (counts', _) <- runTestText nullAccum t
    return counts'
    where
        nullAccum = PutText (\ _ _ _ -> return ()) ()

-- Some combinations of HUnit, GHC, and optimization levels cause tests to be
-- optimized away.  This section verifies that all tests of a type are
-- performed.
optimizationTests :: IO ()
optimizationTests = do
    counts2 <- simpleTestRunner $ TestLabel "Basic Optimization Tests" $ TestList [ 
        True ~=? True,
        False ~=? True,
        TestCase $ assertEqual "both true" True True,
        TestCase $ assertEqual "false true" False True,
        TestCase $ assertEqual "fa" False True,
        TestCase $ assertEqual "f" False True,
        TestCase $ (False @?= True),
        TestCase $ unless (False == True) (assertFailure "f")
        ]
    -- Verify results of counts2
    -- We can't use HUnit because it's possible that some tests have been
    -- optimized away, so we'll just do it manually.
    unless 
        (cases counts2 == 8)
        (putStrLn $ "Failure: Basic Optimization (" ++ optimizationLevel ++ "): expected 8 test cases; only " ++ (show $ cases counts2) ++ " found.  Some may have been optimized out.")
    unless 
        (tried counts2 == 8)
        (putStrLn $ "Failure: Basic Optimization (" ++ optimizationLevel ++ "): expected to try 8 test cases; only " ++ (show $ tried counts2) ++ " tried.  Some may have been optimized out.")
    unless 
        (errors counts2 == 0)
        (putStrLn $ "Failure: Basic Optimization (" ++ optimizationLevel ++ "): expected 0 errors; " ++ (show $ errors counts2) ++ " found.")
    unless
        (failures counts2 == 6)
        (putStrLn $ "Failure: Basic Optimization (" ++ optimizationLevel ++ "): expected 6 failed cases; only " ++ (show $ failures counts2) ++ " failed.  Some may have been optimized out.")
    return ()

-- Added in 1.4.2.3
-- When certain errors occur in a list of tests, the subsequent tests in the
-- list weren't being run.  This test verifies that this does not happen.
undefinedSwallowsTests :: IO ()
undefinedSwallowsTests = do
    -- Added in 1.2.4.3 because the second test case will never be run
    -- (in prior versions)
    counts2 <- simpleTestRunner . TestList $ [
        TestCase $ ('f' : undefined) @?= "bar",
        TestCase $ "foo" @?= "bar"
        ]
    _ <- runTestTT $ TestLabel ("Undefined Swallows Tests (" ++ optimizationLevel ++ ")") $ TestList [
        TestCase $ assertEqual "cases" (cases counts2) 2, 
        TestCase $ assertEqual "tried" (tried counts2) 2, 
        TestCase $ assertEqual "errors" (errors counts2) 1, 
        TestCase $ assertEqual "failures" (failures counts2) 1
        ]
    return ()

-- A simple sequencer of the 2 tests
main :: IO ()
main = do
    optimizationTests
    undefinedSwallowsTests
