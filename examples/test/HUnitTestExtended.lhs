HUnitTestExc.lhs  --  test for HUnit, using Haskell language system "Exc"

> module Main (main) where

> import Test.HUnit
> import HUnitTestBase

 import qualified Control.Exception (assert)

 assertionMessage = "HUnitTestExc.lhs:13: Assertion failed\n"
 assertion = Control.Exception.assert False (return ())


> main :: IO Counts
> main = runTestTT (test [baseTests, excTests])

> excTests :: Test
> excTests = test [

    -- Hugs and GHC don't currently catch arithmetic exceptions.
   "div by 0" ~:
     expectUnspecifiedError (TestCase ((3 `div` 0) `seq` return ())),

    -- GHC doesn't currently catch array-related exceptions.
   "array ref out of bounds" ~:
     expectUnspecifiedError (TestCase (... `seq` return ())),

>   "error" ~:
>     expectError "error" (TestCase (error "error")),

>   "tail []" ~:
>     expectUnspecifiedError (TestCase (tail [] `seq` return ()))

   -- Hugs doesn't provide `assert`.
   "assert" ~:
     expectError assertionMessage (TestCase assertion)

>  ]
