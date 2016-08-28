{-# LANGUAGE CPP #-}
module HUnitTestExtended (
    extendedTests
    ) where

import Test.HUnit
import HUnitTestBase

#if MIN_VERSION_base(4,9,0)
errorCall :: a
errorCall = error "error"
#endif

extendedTests :: Test
extendedTests = test [

    -- Hugs doesn't currently catch arithmetic exceptions.

    "div by 0" ~:
        expectUnspecifiedError (TestCase ((3 `div` 0 :: Integer) `seq` return ())),

    "list ref out of bounds" ~:
        expectUnspecifiedError (TestCase ([1 .. 4 :: Integer] !! 10 `seq` return ())),

#if MIN_VERSION_base(4,9,0)
    "error" ~:
        expectError "error\nCallStack (from HasCallStack):\n  error, called at tests/HUnitTestExtended.hs:11:13 in main:HUnitTestExtended" (TestCase errorCall),
#else
     "error" ~:
        expectError "error" (TestCase (error "error")),
#endif

    "tail []" ~:
        expectUnspecifiedError (TestCase (tail [] `seq` return ()))
    ]
