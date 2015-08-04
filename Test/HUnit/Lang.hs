{-# LANGUAGE DeriveDataTypeable #-}
module Test.HUnit.Lang (
  Assertion,
  assertFailure,
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

-- | When an assertion is evaluated, it will output a message if and only if the
-- assertion fails.
--
-- Test cases are composed of a sequence of one or more assertions.
type Assertion = IO ()

data HUnitFailure = HUnitFailure String
    deriving (Show, Typeable)

instance Exception HUnitFailure

-- | Unconditionally signals that a failure has occured.  All
-- other assertions can be expressed with the form:
--
-- @
--    if conditionIsMet
--        then IO ()
--        else assertFailure msg
-- @
assertFailure :: String -- ^ A message that is displayed with the assertion failure
              -> Assertion
assertFailure msg = msg `deepseq` E.throwIO (HUnitFailure msg)

-- | Performs a single test case.  The meaning of the result is as follows:
--
--     [@Nothing@]           test case success
--
--     [@Just (True,  msg)@] test case failure with the given message
--
--     [@Just (False, msg)@] test case error with the given message
performTestCase :: Assertion -- ^ an assertion to be made during the test case run
                -> IO (Maybe (Bool, String))
performTestCase action =
    do action
       return Nothing
     `E.catches`
      [E.Handler (\(HUnitFailure msg) -> return $ Just (True, msg)),

       -- Re-throw AsyncException, otherwise execution will not terminate on
       -- SIGINT (ctrl-c).  Currently, all AsyncExceptions are being thrown
       -- because it's thought that none of them will be encountered during
       -- normal HUnit operation.  If you encounter an example where this
       -- is not the case, please email the maintainer.
       E.Handler (\e -> throw (e :: E.AsyncException)),

       E.Handler (\e -> return $ Just (False, show (e :: E.SomeException)))]
