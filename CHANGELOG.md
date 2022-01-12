# Changelog

## 0.0.3

_2022-01-12_

* Streamline generics module API (#2)
* Add number.lessThanEqual and .greaterThanEqual
* Add Cursor.fromValidatedNel
* Remove validations traits and only provide objects
* Distinguish between total mapping and partial mapping
* Fetch entire repo in CI
* Introduce module builder
* Upgrade to scala 2.13.8

## 0.0.2

_2022-01-10_

* Add `prepend` (`/:`) operations to `Selection.History`
* Add `Validation.collect`
* Add `Validation.mapping`
* Add `CursorValidation.unnamedOneOf`
* Add `Validation.ask`
* Use `Chain` (instead of `List`) as the underlying data structure for `Selection.History`

## 0.0.1

_2022-01-09_

* Initial release