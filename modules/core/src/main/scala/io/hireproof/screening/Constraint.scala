package io.hireproof.screening

import cats.Show
import cats.syntax.all._

import java.time.ZonedDateTime
import scala.Numeric.Implicits._
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

sealed abstract class Constraint extends Product with Serializable {
  def toDebugString: String
}

object Constraint {
  final case class Or(left: Set[Constraint], right: Set[Constraint]) extends Constraint {
    override def toDebugString: String =
      left.map(_.toDebugString).mkString("[", ", ", "]") + " || " + right.map(_.toDebugString).mkString("[", ", ", "]")
  }

  final case class Value(
      identifier: Constraint.Identifier,
      reference: Option[Reference],
      delta: Option[Constraint.Delta],
      equal: Option[Boolean]
  ) extends Constraint {
    override def toDebugString: String = {
      val parameters = reference.map(reference => s"reference=${reference.value}").toList ++
        delta.map(delta => s"delta=${delta.value}").toList ++
        equal.map(equal => s"equal=$equal").toList
      s"${identifier.value}(${parameters.mkString(", ")})"
    }
  }

  final case class Identifier(value: String) extends AnyVal

  object Identifier {
    val After: Constraint.Identifier = Identifier("after")
    val Before: Constraint.Identifier = Identifier("before")
    val Contains: Constraint.Identifier = Identifier("contains")
    val Email: Constraint.Identifier = Identifier("email")
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

    def fromNumeric[A: Numeric](value: A): Delta = {
      val double = value.toDouble
      Delta(String.format(if (double % 1.0d != 0) "%s" else "%.0f", double))
    }
  }

  def apply(identifier: Identifier, reference: Reference, delta: Delta, equal: Boolean): Constraint =
    Value(identifier, reference.some, delta.some, equal.some)
  def apply(identifier: Identifier, reference: Reference, delta: Delta): Constraint =
    Value(identifier, reference.some, delta.some, equal = none)
  def apply(identifier: Identifier, reference: Reference, equal: Boolean): Constraint =
    Value(identifier, reference.some, delta = none, equal.some)
  def apply(identifier: Identifier, reference: Reference): Constraint =
    Value(identifier, reference.some, delta = none, equal = none)
  def apply(identifier: Identifier): Constraint = Value(identifier, reference = none, delta = none, equal = none)

  object collection {
    def contains[A: Show](reference: A): Constraint = Constraint(Identifier.Contains, Reference.fromShow(reference))
  }

  object duration {
    def equal(reference: FiniteDuration): Constraint =
      Constraint(Identifier.Equal, Reference.fromShow(reference))

    def greaterThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.GreaterThan, Reference.fromShow(reference), equal)

    def lessThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.LessThan, Reference.fromShow(reference), equal)
  }

  object number {
    def equal[A: Numeric](reference: A, delta: A): Constraint =
      Constraint(Identifier.Equal, Reference.fromNumeric(reference), Delta.fromNumeric(delta))

    def greaterThan[A: Numeric](reference: A, delta: A, equal: Boolean): Constraint =
      Constraint(Identifier.GreaterThan, Reference.fromNumeric(reference), Delta.fromNumeric(delta), equal)

    def lessThan[A: Numeric](reference: A, delta: A, equal: Boolean): Constraint =
      Constraint(Identifier.LessThan, Reference.fromNumeric(reference), Delta.fromNumeric(delta), equal)
  }

  def oneOf[A: Show](references: Set[A]): Constraint =
    Constraint(Identifier.OneOf, Reference.fromIterable(references))

  object optional {
    val isDefined: Constraint = Constraint(Identifier.Required)
  }

  def parsing(name: String): Constraint = Constraint(Identifier(name))

  object text {
    val email: Constraint = Constraint(Identifier.Email)

    def equal(reference: String): Constraint = Constraint(Identifier.Equal, Reference(reference))

    def matches(regex: Regex): Constraint = Constraint(Identifier.Matches, Reference(regex.regex))
  }

  object time {
    def after(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.After, Reference(reference.toString), equal)

    def before(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.Before, Reference(reference.toString), equal)
  }
}
