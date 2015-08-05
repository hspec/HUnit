module HUnitTestExtended (
    extendedTests
    ) where

import Test.HUnit
import HUnitTestBase

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
    ]
