package io.hireproof.screening

import cats.syntax.all._
import io.hireproof.openapi.{Encoder, OpenApi}
import io.hireproof.openapi.syntax._

import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

sealed abstract class Constraint extends Product with Serializable {
  def toDebugString: String

  override def toString: String = toDebugString
}

object Constraint {
  final case class Or(left: Set[Constraint], right: Set[Constraint]) extends Constraint {
    override def toDebugString: String =
      left.map(_.toDebugString).mkString("[", ", ", "]") + " || " + right.map(_.toDebugString).mkString("[", ", ", "]")
  }

  final case class Rule(
      identifier: Constraint.Identifier,
      reference: Option[OpenApi],
      delta: Option[Double],
      equal: Option[Boolean]
  ) extends Constraint {
    override def toDebugString: String = {
      val parameters = reference.map(reference => s"reference=$reference").toList ++
        delta.map(delta => s"delta=${String.format(if (delta % 1.0d != 0) "%s" else "%.0f", delta)}").toList ++
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

  def apply[A: Encoder](identifier: Identifier, reference: A, delta: Double, equal: Boolean): Constraint =
    Rule(identifier, reference.asOpenApi.some, delta.some, equal.some)
  def apply[A: Encoder](identifier: Identifier, reference: A, delta: Double): Constraint =
    Rule(identifier, reference.asOpenApi.some, delta.some, equal = none)
  def apply[A: Encoder](identifier: Identifier, reference: A, equal: Boolean): Constraint =
    Rule(identifier, reference.asOpenApi.some, delta = none, equal.some)
  def apply[A: Encoder](identifier: Identifier, reference: A): Constraint =
    Rule(identifier, reference.asOpenApi.some, delta = none, equal = none)
  def apply(identifier: Identifier): Constraint = Rule(identifier, reference = none, delta = none, equal = none)

  object collection {
    def contains[A: Encoder](reference: A): Constraint = Constraint(Identifier.Contains, reference)
  }

  object duration {
    def equal(reference: FiniteDuration): Constraint = Constraint(Identifier.Equal, reference)
    def greaterThan(reference: FiniteDuration, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, reference, equal)
    def lessThan(reference: FiniteDuration, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, reference, equal)
  }

  object number {
    def equal[A: Encoder](reference: A, delta: Double = 0d): Constraint =
      Constraint(Identifier.Equal, reference, delta)
    def greaterThan[A: Encoder](reference: A, delta: Double = 0d, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, reference, delta, equal)
    def lessThan[A: Encoder](reference: A, delta: Double = 0d, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, reference, delta, equal)
  }

  def oneOf[A: Encoder](references: Set[A]): Constraint = Constraint(Identifier.OneOf, references)

  def parsing(name: String): Constraint = Constraint(Identifier(name))

  object text {
    val email: Constraint = Constraint(Identifier.Email)
    def equal(reference: String): Constraint = Constraint(Identifier.Equal, OpenApi.fromString(reference))
    def matches(regex: Regex): Constraint = Constraint(Identifier.Matches, OpenApi.fromString(regex.regex))
  }

  val required: Constraint = Constraint(Identifier.Required)

  object time {
    def after(reference: ZonedDateTime, equal: Boolean = true): Constraint =
      Constraint(Identifier.After, OpenApi.fromString(reference.toString), equal)
    def before(reference: ZonedDateTime, equal: Boolean = true): Constraint =
      Constraint(Identifier.Before, OpenApi.fromString(reference.toString), equal)
  }
}
