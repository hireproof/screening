package io.hireproof.screening

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Encoder, Json}

sealed abstract class Violation extends Product with Serializable {
  def toConstraint: Option[Constraint]

  def toActual: Option[Json]
}

object Violation {
  final case class Validation(constraint: Constraint, actual: Json) extends Violation {
    override def toConstraint: Option[Constraint] = constraint.some
    override def toActual: Option[Json] = actual.some
  }

  final case class Conflict(actual: Json) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[Json] = actual.some
  }

  final case class Invalid(reference: Option[Json], actual: Json) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[Json] = actual.some
  }

  final case class Missing(reference: Option[Json]) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[Json] = none
  }

  final case class Unknown(actual: Json) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[Json] = actual.some
  }

  def apply[A: Encoder](constraint: Constraint, actual: A): Violation = Validation(constraint, actual.asJson)
  def conflict[A: Encoder](actual: A): Violation = Conflict(actual.asJson)
  def invalid[A: Encoder](reference: A, actual: A): Violation = Invalid(reference.asJson.some, actual.asJson)
  def invalid[A: Encoder](actual: A): Violation = Invalid(reference = none, actual.asJson)
  val missing: Violation = Missing(reference = none)
  def missing[A: Encoder](reference: A): Violation = Missing(reference.asJson.some)
  def unknown[A: Encoder](actual: A): Violation = Unknown(actual.asJson)
}
