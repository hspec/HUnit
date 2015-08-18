# HUnit

HUnit is a unit testing framework for Haskell, inspired by the JUnit
tool for Java.  HUnit is free software; see its "License" file for
details.  HUnit is available at <http://hunit.sourceforge.net>.

HUnit 1.1.1 consists of a number of files.  Besides Haskell source files
in Test/HUnit (whose names end in ".hs" or ".lhs"), these files include:

```
  * README.md       -- this file
  * doc/Guide.html  -- user's guide, in HTML format
  * LICENSE         -- license for use of HUnit
```

See the user's guide for more information.

## Changes

### 1.3.0.0

- removed support for old compilers

- add source locations for failing assertions (GHC >= 7.10.2 only)
