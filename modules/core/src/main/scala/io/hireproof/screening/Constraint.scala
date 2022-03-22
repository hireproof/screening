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

  final case class Rule(
      identifier: Constraint.Identifier,
      reference: Option[Value],
      delta: Option[Number],
      equal: Option[Boolean]
  ) extends Constraint {
    override def toDebugString: String = {
      val parameters = reference.map(reference => s"reference=$reference").toList ++
        delta.map(delta => s"delta=$delta").toList ++
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

  def apply(identifier: Identifier, reference: Value, delta: Number, equal: Boolean): Constraint =
    Rule(identifier, reference.some, delta.some, equal.some)
  def apply(identifier: Identifier, reference: Value, delta: Number): Constraint =
    Rule(identifier, reference.some, delta.some, equal = none)
  def apply(identifier: Identifier, reference: Value, equal: Boolean): Constraint =
    Rule(identifier, reference.some, delta = none, equal.some)
  def apply(identifier: Identifier, reference: Value): Constraint =
    Rule(identifier, reference.some, delta = none, equal = none)
  def apply(identifier: Identifier): Constraint = Rule(identifier, reference = none, delta = none, equal = none)

  object collection {
    def contains[A: Show](reference: A): Constraint = Constraint(Identifier.Contains, Value.fromShow(reference))
  }

  object duration {
    def equal(reference: FiniteDuration): Constraint =
      Constraint(Identifier.Equal, Value.fromShow(reference))

    def greaterThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.GreaterThan, Value.fromShow(reference), equal)

    def lessThan(reference: FiniteDuration, equal: Boolean): Constraint =
      Constraint(Identifier.LessThan, Value.fromShow(reference), equal)
  }

  object number {
    def equal(reference: BigDecimal, delta: BigDecimal = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromBigDecimal(reference), Number.fromBigDecimal(delta))

    def greaterThan(reference: BigDecimal, delta: BigDecimal = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromBigDecimal(reference), Number.fromBigDecimal(delta), equal)

    def lessThan(reference: BigDecimal, delta: BigDecimal = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromBigDecimal(reference), Number.fromBigDecimal(delta), equal)

    def equal(reference: BigInt, delta: BigInt = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromBigInt(reference), Number.fromBigInt(delta))

    def greaterThan(reference: BigInt, delta: BigInt = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromBigInt(reference), Number.fromBigInt(delta), equal)

    def lessThan(reference: BigInt, delta: BigInt = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromBigInt(reference), Number.fromBigInt(delta), equal)

    def equal(reference: Double, delta: Double = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromDouble(reference), Number.fromDouble(delta))

    def greaterThan(reference: Double, delta: Double = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromDouble(reference), Number.fromDouble(delta), equal)

    def lessThan(reference: Double, delta: Double= 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromDouble(reference), Number.fromDouble(delta), equal)

    def equal(reference: Float, delta: Float = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromFloat(reference), Number.fromFloat(delta))

    def greaterThan(reference: Float, delta: Float = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromFloat(reference), Number.fromFloat(delta), equal)

    def lessThan(reference: Float, delta: Float= 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromFloat(reference), Number.fromFloat(delta), equal)

    def equal(reference: Int, delta: Int = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromInt(reference), Number.fromInt(delta))

    def greaterThan(reference: Int, delta: Int = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromInt(reference), Number.fromInt(delta), equal)

    def lessThan(reference: Int, delta: Int= 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromInt(reference), Number.fromInt(delta), equal)

    def equal(reference: Long, delta: Long = 0): Constraint =
      Constraint(Identifier.Equal, Number.fromLong(reference), Number.fromLong(delta))

    def greaterThan(reference: Long, delta: Long = 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.GreaterThan, Number.fromLong(reference), Number.fromLong(delta), equal)

    def lessThan(reference: Long, delta: Long= 0, equal: Boolean = true): Constraint =
      Constraint(Identifier.LessThan, Number.fromLong(reference), Number.fromLong(delta), equal)
  }

  def oneOf[A: Show](references: Set[A]): Constraint =
    Constraint(Identifier.OneOf, Value.fromList(references.map(Value.fromShow[A]).toList))

  object optional {
    val isDefined: Constraint = Constraint(Identifier.Required)
  }

  def parsing(name: String): Constraint = Constraint(Identifier(name))

  object text {
    val email: Constraint = Constraint(Identifier.Email)

    def equal(reference: String): Constraint = Constraint(Identifier.Equal, Value.fromString(reference))

    def matches(regex: Regex): Constraint = Constraint(Identifier.Matches, Value.fromString(regex.regex))
  }

  object time {
    def after(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.After, Value.fromString(reference.toString), equal)

    def before(reference: ZonedDateTime, equal: Boolean): Constraint =
      Constraint(Identifier.Before, Value.fromString(reference.toString), equal)
  }
}
