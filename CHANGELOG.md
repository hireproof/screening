# Changelog

## 0.0.17

_2022-04-19_

* Add `Validation.option`
* Remove `Validation.forAll`
* Update sbt-houserules to 0.3.22 (#15)
* Update scala3-library, ... to 3.1.2 (#14)

## 0.0.16

_2022-04-11_

* Add json validation rules
* Add parsing.json
* Update sbt-scalajs, scalajs-compiler, ... to 1.10.0 (#12)

## 0.0.15

_2022-03-23_

* Add equal = true to duration constraint builders
* Fix Violations.merge
* Fix constraint rule decoder
* Improve Constraint.toString
* Fix Validation.withConstraint generator wrong Violations
* Serialization (#11)
* Update sbt-scalajs-crossproject to 1.2.0 (#10)
* Only allow Strings in Violation helpers, not Any

## 0.0.14

_2022-03-22_

* Rename `BrokenConstraint` to `Validation`
* Remove generic parameter from `Violations` and rename `Error` to `Violation`
* Add `Semigroup` instance for `Violations`
* `email` validation

## 0.0.13

_2022-03-22_

* Overall structure rework switching to loosely typed constraints to increase flexibility

## 0.0.12

_2022-03-10_

* __ -> hist
* Add parsing.catchOnly[T]

## 0.0.11

_2022-03-08_

* Use 2 arg groups for `parsing.apply`
* Add parsing.boolean
* Update sbt-houserules to 0.3.20 (#6)

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