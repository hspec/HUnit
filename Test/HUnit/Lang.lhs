Test/HUnit/Lang.lhs  --  HUnit language support.

> module Test.HUnit.Lang
> (
>   Assertion,
>   assertFailure,
>   performTestCase
> )
> where


When adapting this module for other Haskell language systems, change
the imports and the implementations but not the interfaces.



Imports
-------

> import Data.List (isPrefixOf)
#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
> import Data.Dynamic
> import Control.Exception as E         ( throwDyn, try, Exception(..) )
#else
> import System.IO.Error (ioeGetErrorString, try)
#endif



Interfaces
----------

An assertion is an `IO` computation with trivial result.

> type Assertion = IO ()

`assertFailure` signals an assertion failure with a given message.

> assertFailure :: String -> Assertion

`performTestCase` performs a single test case.  The meaning of the
result is as follows:
  Nothing               test case success
  Just (True,  msg)     test case failure with the given message
  Just (False, msg)     test case error with the given message

> performTestCase :: Assertion -> IO (Maybe (Bool, String))


Implementations
---------------

#if defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
> data HUnitFailure = HUnitFailure String
>
> hunitFailureTc :: TyCon
> hunitFailureTc = mkTyCon "HUnitFailure"
> {-# NOINLINE hunitFailureTc #-}
> 
> instance Typeable HUnitFailure where
>     typeOf _ = mkTyConApp hunitFailureTc []

> assertFailure msg = E.throwDyn (HUnitFailure msg)

> performTestCase action = 
>     do r <- E.try action
>        case r of 
>          Right () -> return Nothing
>          Left e@(E.DynException dyn) -> 
>              case fromDynamic dyn of
>                Just (HUnitFailure msg) -> return $ Just (True, msg)
>                Nothing                 -> return $ Just (False, show e)
>          Left e -> return $ Just (False, show e)
#else
> hunitPrefix = "HUnit:"

> nhc98Prefix = "I/O error (user-defined), call to function `userError':\n  "

> assertFailure msg = ioError (userError (hunitPrefix ++ msg))

> performTestCase action = do r <- try action
>                             case r of Right () -> return Nothing
>                                       Left  e  -> return (Just (decode e))
>  where
>   decode e = let s0 = ioeGetErrorString e
>                  (_, s1) = dropPrefix nhc98Prefix s0
>              in            dropPrefix hunitPrefix s1
>   dropPrefix pref str = if pref `isPrefixOf` str
>                           then (True, drop (length pref) str)
>                           else (False, str)
#endif
