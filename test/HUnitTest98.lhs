HUnitTest98.lhs  --  test for HUnit, using Haskell language system "98"

$Id: HUnitTest98.lhs,v 1.1 2004/03/26 11:23:10 malcolm Exp $

> module Main (main) where

> import Test.HUnit
> import HUnitTestBase


> main = runTestTT (test [baseTests])
