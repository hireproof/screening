# Changelog

## 0.0.10

_2022-02-19_

* Relax parsing constraints to be open to any `f: String => Option[A]`
* Add `Error.Missing`
* Update sbt-scalajs, scalajs-compiler, ... to 1.9.0 (#5)

## 0.0.9

_2022-02-09_

* Always normalize validations before run

## 0.0.8

_2022-02-08_

* Add Validation debug printing
* Update scala3 to 3.1.1 (#4)
* Update sbt to 1.6.2 (#3)

## 0.0.7

_2022-01-24_

* Add `Validation.Error.Invalid`

## 0.0.6

_2022-01-18_

* Add UUID parsing rule
* Disable scalajs platform for root module

## 0.0.5

_2022-01-16_

* Move `Cursor.Errors` to `Validation.Errors`

## 0.0.4

_2022-01-15_

* Remove `ValidatingDecoder`
* Move `Selection` class into `core` module
* Check `Validation` `Arrow` laws
* Upgrade to sbt-ci-release 1.5.10
* Upgrade to sbt-houserules 0.3.19

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