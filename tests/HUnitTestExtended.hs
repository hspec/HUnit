module HUnitTestExtended (
    extendedTests
    ) where

import Test.HUnit
import HUnitTestBase

-- Notes:
-- * Assertion testing is only performed on GHC.  If you want this to be enabled
--   for other compilers, email the HUnit maintainer.

#ifdef __GLASGOW_HASKELL__
import qualified Control.Exception (assert)

#ifdef O0
import System.FilePath

assertionMessage :: String
assertionMessage = concat [
    "tests",
    [pathSeparator],
    "HUnitTestExtended.hs:27:13-36: Assertion failed\n"
    ]
#endif

assertion :: IO ()
assertion = Control.Exception.assert False (return ())
#endif


extendedTests :: Test
extendedTests = test [

    -- Hugs doesn't currently catch arithmetic exceptions.
    
    "div by 0" ~:
        expectUnspecifiedError (TestCase ((3 `div` 0 :: Integer) `seq` return ())),

    "list ref out of bounds" ~:
        expectUnspecifiedError (TestCase ([1 .. 4 :: Integer] !! 10 `seq` return ())),

    "error" ~:
        expectError "error" (TestCase (error "error")),

    "tail []" ~:
        expectUnspecifiedError (TestCase (tail [] `seq` return ()))

#ifdef __GLASGOW_HASKELL__
#ifdef O0
-- Run with no optimization (-O0)
    ,
    "assert" ~:
         expectError assertionMessage (TestCase assertion)
#else
-- #ifdef O0
-- Run with optimization (-O1 or -O2)
    ,
    "assert" ~: TestCase assertion
-- #ifdef O0
#endif
-- #ifdef __GLASGOW_HASKELL__
#endif
    ]
