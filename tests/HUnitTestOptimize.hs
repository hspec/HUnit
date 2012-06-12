-- HUnitTestOptimize.hs
--
-- The purpose of this file is to test whether certain issues occur with optimization.
-- It should be built and run with each level of optimization.  I.e., -O0, -O1, -O2

module Main (main) where

import Control.Monad (unless)
import Test.HUnit

simpleTestRunner :: Test -> IO Counts
simpleTestRunner t = do 
    (counts', _) <- runTestText nullAccum t
    return counts'
    where
        nullAccum = PutText (\ _ _ _ -> return ()) ()

main :: IO Counts
main = do
    counts2 <- simpleTestRunner $ TestList [ 
        True ~=? True,
        False ~=? True,
        TestCase $ assertEqual "both true" True True,
        TestCase $ assertEqual "false true" False True,
        TestCase $ assertEqual "fa" False True,
        TestCase $ assertEqual "f" False True,
        TestCase $ (False @?= True),
        TestCase $ unless (False == True) (assertFailure "f")
        ]
    counts3 <- runTestTT $ TestList [
        TestCase $ assertEqual "Number of cases" (cases counts2) 8,
        TestCase $ assertEqual "Number of cases tried" (tried counts2) 8,
        TestCase $ assertEqual "Number of failures" (failures counts2) 6
        ]
    -- Added in 1.2.4.3 because the second test case will never be run
    -- (in prior versions)
    putStrLn "There should be 3 cases, 3 tried, and 0 errors and 0 failures."
    counts4 <- runTestTT . TestList $ [
        TestCase $ ('f' : undefined) @?= "bar",
        TestCase $ "foo" @?= "bar"
        ]
    putStrLn "There should be 2 cases, 2 tried, and 1 errors and 1 failures."
    return counts3
