-- HUnitTestOptimize.hs
--
-- The purpose of this file is to test whether certain issues occur with optimization.
-- It should be built and run with each level of optimization.  I.e., -O0, -O1, -O2
--
-- With some versions and optimization levels of HUnit and GHC, tests were getting
-- optimized out.  This is a very bad thing and needs to be tested for.

module HUnitTestOptimize (
    optimizationTests,
    undefinedSwallowsTests
    ) where

import Control.Applicative
import Control.Monad
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
optimizationTests :: IO Bool
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
    foldr (&&) True <$> sequence [
        caseCount counts2 optimizationLevel, 
        tryCount counts2 optimizationLevel, 
        errorCount counts2 optimizationLevel, 
        failureCount counts2 optimizationLevel
        ]
    where
        caseCount cs ol = if (cases cs == 8) 
                    then return True
                    else do
                        putStrLn $ "Failure: Basic Optimization (" ++ 
                                ol ++ "): expected 8 test cases; only " ++ 
                                (show $ cases cs) ++ 
                                " found.  Some may have been optimized out."
                        return False
        tryCount cs ol = if (tried cs == 8) 
                    then return True
                    else do
                        putStrLn $ "Failure: Basic Optimization (" ++ 
                            ol ++ "): expected to try 8 test cases; only " ++ 
                            (show $ tried cs) ++ 
                            " tried.  Some may have been optimized out."
                        return False
        errorCount cs ol = if (errors cs == 0) 
                    then return True
                    else do
                        putStrLn $ "Failure: Basic Optimization (" ++ 
                            ol ++ "): expected 0 errors; " ++ 
                            (show $ errors cs) ++ " found."
                        return False
        failureCount cs ol = if (failures cs == 6) 
                    then return True
                    else do
                        putStrLn $ "Failure: Basic Optimization (" ++ 
                            ol ++ "): expected 6 failed cases; only " ++ 
                            (show $ failures cs) ++ 
                            " failed.  Some may have been optimized out."
                        return False

-- Added in 1.4.2.3
-- When certain errors occur in a list of tests, the subsequent tests in the
-- list weren't being run.  This test verifies that this does not happen.
undefinedSwallowsTests :: Test
undefinedSwallowsTests = TestLabel ("Undefined Swallows Tests (" ++ optimizationLevel ++ ")") $ TestList [
        TestCase $ do
            rs <- simpleTestRunner . TestList $ [
                -- Added in 1.2.4.3 because the second test case will never be run
                -- (in prior versions)
                TestCase $ ('f' : undefined) @?= "bar",
                TestCase $ "foo" @?= "bar"
                ]
            assertEqual 
                "(cases,tried,errors,failures)" 
                (cases rs, tried rs, errors rs, failures rs)
                (2, 2, 1, 1)
        ]