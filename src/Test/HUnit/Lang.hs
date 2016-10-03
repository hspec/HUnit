{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.HUnit.Lang (
  Assertion,
  assertFailure,

  Result (..),
  performTestCase,
-- * Internals
-- |
-- /Note:/ This is not part of the public API!  It is exposed so that you can
-- tinker with the internals of HUnit, but do not expect it to be stable!
  HUnitFailure (..)
) where

import           Control.DeepSeq
import           Control.Exception as E
import           Data.Typeable
import           Data.CallStack

-- | When an assertion is evaluated, it will output a message if and only if the
-- assertion fails.
--
-- Test cases are composed of a sequence of one or more assertions.
type Assertion = IO ()

data HUnitFailure = HUnitFailure (Maybe SrcLoc) String
    deriving (Eq, Show, Typeable)

instance Exception HUnitFailure

-- | Unconditionally signals that a failure has occured.  All
-- other assertions can be expressed with the form:
--
-- @
--    if conditionIsMet
--        then IO ()
--        else assertFailure msg
-- @
assertFailure ::
     HasCallStack =>
     String -- ^ A message that is displayed with the assertion failure
  -> Assertion
assertFailure msg = msg `deepseq` E.throwIO (HUnitFailure location msg)
  where
    location :: Maybe SrcLoc
    location = case reverse callStack of
      (_, loc) : _ -> Just loc
      [] -> Nothing

data Result = Success | Failure (Maybe SrcLoc) String | Error (Maybe SrcLoc) String
  deriving (Eq, Show)

-- | Performs a single test case.
performTestCase :: Assertion -- ^ an assertion to be made during the test case run
                -> IO Result
performTestCase action =
  (action >> return Success)
     `E.catches`
      [E.Handler (\(HUnitFailure loc msg) -> return $ Failure loc msg),

       -- Re-throw AsyncException, otherwise execution will not terminate on
       -- SIGINT (ctrl-c).  Currently, all AsyncExceptions are being thrown
       -- because it's thought that none of them will be encountered during
       -- normal HUnit operation.  If you encounter an example where this
       -- is not the case, please email the maintainer.
       E.Handler (\e -> throw (e :: E.AsyncException)),

       E.Handler (\e -> return $ Error Nothing $ show (e :: E.SomeException))]
