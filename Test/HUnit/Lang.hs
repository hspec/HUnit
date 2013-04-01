-- | This module abstracts the differences between implementations of 
-- Haskell (e.g., GHC, Hugs, and NHC).

{-# LANGUAGE CPP #-} 
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
#endif

module Test.HUnit.Lang
(
  Assertion,
  assertFailure,
  performTestCase,
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
-- * Internals
-- |
-- /Note:/ This is not part of the public API!  It is exposed so that you can
-- tinker with the internals of HUnit, but do not expect it to be stable!
  HUnitFailure (..)
#endif
)
where


-- When adapting this module for other Haskell language systems, change
-- the imports and the implementations but not the interfaces.



-- Imports
-- -------

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import Data.Dynamic
import Control.Exception as E
#else
import Data.List (isPrefixOf)
import System.IO.Error (ioeGetErrorString, try)
#endif

import Control.DeepSeq


-- Interfaces
-- ----------

-- | When an assertion is evaluated, it will output a message if and only if the
-- assertion fails.  
--
-- Test cases are composed of a sequence of one or more assertions.

type Assertion = IO ()

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

-- | Performs a single test case.  The meaning of the result is as follows:
--
--     [@Nothing@]           test case success
--
--     [@Just (True,  msg)@] test case failure with the given message
--
--     [@Just (False, msg)@] test case error with the given message

performTestCase :: Assertion -- ^ an assertion to be made during the test case run 
                -> IO (Maybe (Bool, String))


-- Implementations
-- ---------------

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
data HUnitFailure = HUnitFailure String
#if __GLASGOW_HASKELL__ >= 707
    deriving (Show, Typeable)
#else
    deriving Show

hunitFailureTc :: TyCon
#if MIN_VERSION_base(4,4,0)
hunitFailureTc = mkTyCon3 "HUnit" "Test.HUnit.Lang" "HUnitFailure"
#else
hunitFailureTc = mkTyCon "HUnitFailure"
#endif
{-# NOINLINE hunitFailureTc #-}
 
instance Typeable HUnitFailure where
    typeOf _ = mkTyConApp hunitFailureTc []
#endif

#ifdef BASE4
instance Exception HUnitFailure

assertFailure msg = msg `deepseq` E.throwIO (HUnitFailure msg)

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
#else
assertFailure msg = msg `deepseq` E.throwDyn (HUnitFailure msg)

performTestCase action = 
    do r <- E.try action
       case r of 
         Right () -> return Nothing
         Left e@(E.DynException dyn) -> 
             case fromDynamic dyn of
               Just (HUnitFailure msg) -> return $ Just (True, msg)
               Nothing                 -> return $ Just (False, show e)
         Left e -> return $ Just (False, show e)
#endif
#else
hunitPrefix = "HUnit:"

nhc98Prefix = "I/O error (user-defined), call to function `userError':\n  "

assertFailure msg = msg `deepseq` ioError (userError (hunitPrefix ++ msg))

performTestCase action = do r <- try action
                            case r of Right () -> return Nothing
                                      Left  e  -> return (Just (decode e))
 where
  decode e = let s0 = ioeGetErrorString e
                 (_, s1) = dropPrefix nhc98Prefix s0
             in            dropPrefix hunitPrefix s1
  dropPrefix pref str = if pref `isPrefixOf` str
                          then (True, drop (length pref) str)
                          else (False, str)
#endif
