Test/HUnit/Lang.lhs  --  HUnit language support.

Module `Test.HUnit.Lang` is a mere redirection to *one* of the following:
    Test.HUnit.Lang98
    Test.HUnit.LangExtended

$Id: Lang.lhs,v 1.1 2004/03/26 11:23:09 malcolm Exp $

> module Test.HUnit.Lang
> (
>   Assertion,
>   assertFailure,
>   performTestCase
> )
> where


Import for re-export
--------------------

> import Test.HUnit.Lang98

The alternative is:

  import Test.HUnit.LangExtended

