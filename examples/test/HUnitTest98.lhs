HUnitTest98.lhs  --  test for HUnit, using Haskell language system "98"

$Id: HUnitTest98.lhs,v 1.1 2004/03/27 14:07:34 panne Exp $

> module Main (main) where

> import Test.HUnit
> import HUnitTestBase


> main = runTestTT (test [baseTests])
