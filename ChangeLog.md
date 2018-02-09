# Revision history for `paths`

## 0.2

* Make `Path` abstract by default and move type-unsafe operations into new `System.Path.Unsafe` module
* Add wrappers for `Data.Text(.Lazy).IO` now that `text` is bundled with GHC
* Add `appendByteString` & `appendLazyByteString` wrappers
* Add `{has,drop,add}TrailingPathSeparator` operations
* Change types of `joinFragments` and `splitFragments`, and add new 'fragments` smart-constructor.

## 0.1

* First version. Mostly derived from `hackage-security`'s `Hackage.Security.Util.Path`
