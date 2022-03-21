package io.hireproof.screening

import cats.Show
import cats.syntax.all._

import java.time.ZonedDateTime
import scala.Numeric.Implicits._
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

final case class Constraint(
    identifier: Constraint.Identifier,
    reference: Option[Reference],
    delta: Option[Constraint.Delta],
    equal: Option[Boolean]
)

object Constraint {
  final case class Identifier(value: String) extends AnyVal

  object Identifier {
    val After: Constraint.Identifier = Identifier("after")
    val Before: Constraint.Identifier = Identifier("before")
    val Contains: Constraint.Identifier = Identifier("contains")
    val GreaterThan: Constraint.Identifier = Identifier("greaterThan")
    val LessThan: Constraint.Identifier = Identifier("lessThan")
    val Equal: Constraint.Identifier = Identifier("equal")
    val Matches: Constraint.Identifier = Identifier("matches")
    val OneOf: Constraint.Identifier = Identifier("oneOf")
    val Required: Constraint.Identifier = Identifier("required")
  }

  final case class Delta(value: String) extends AnyVal

  object Delta {
    def fromShow[A: Show](value: A): Delta = Delta(value.show)

    def fromNumeric[A: Numeric](value: A): Delta = fromShow(value.toDouble)
  }

  object collection {
    def contains[A: Show](reference: A): Constraint =
      Constraint(Identifier.Contains, Reference.fromShow(reference).some, delta = none, equal = none)
  }

  object duration {
    def equal(reference: FiniteDuration): Constraint =
      Constraint(Identifier.Equal, Reference.fromShow(reference).some, delta = none, equal = none)

    def greaterThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.GreaterThan, Reference.fromShow(reference).some, delta = none, equal.some)

    def lessThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.LessThan, Reference.fromShow(reference).some, delta = none, equal.some)
  }

  object number {
    def equal[A: Numeric](reference: A, delta: A): Constraint =
      Constraint(Identifier.Equal, Reference.fromNumeric(reference).some, Delta.fromNumeric(delta).some, equal = none)

    def greaterThan[A: Numeric](reference: A, delta: A, equal: Boolean): Constraint =
      Constraint(
        Identifier.GreaterThan,
        Reference.fromNumeric(reference).some,
        Delta.fromNumeric(delta).some,
        equal.some
      )

    def lessThan[A: Numeric](reference: A, delta: A, equal: Boolean): Constraint =
      Constraint(Identifier.LessThan, Reference.fromNumeric(reference).some, Delta.fromNumeric(delta).some, equal.some)
  }

  def oneOf[A: Show](references: Set[A]): Constraint =
    Constraint(Identifier.OneOf, Reference.fromIterable(references).some, delta = none, equal = none)

  object optional {
    val isDefined: Constraint = Constraint(Identifier.Required, reference = none, delta = none, equal = none)
  }

  def parsing(name: String): Constraint = Constraint(Identifier(name), reference = none, delta = none, equal = none)

  object text {
    def equal(reference: String): Constraint =
      Constraint(Identifier.Equal, Reference(reference).some, delta = none, equal = none)

    def matches(regex: Regex): Constraint =
      Constraint(Identifier.Matches, Reference(regex.regex).some, delta = none, equal = none)
  }

  object time {
    def after(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.After, Reference(reference.toString).some, delta = none, equal.some)

    def before(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.Before, Reference(reference.toString).some, delta = none, equal.some)
  }
}
