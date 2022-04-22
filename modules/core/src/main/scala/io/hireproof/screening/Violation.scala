package io.hireproof.screening

import cats.syntax.all._
import io.hireproof.openapi.{Encoder, OpenApi}
import io.hireproof.openapi.syntax._

sealed abstract class Violation extends Product with Serializable {
  def toConstraint: Option[Constraint]
  def toActual: Option[OpenApi]
}

object Violation {
  final case class Validation(constraint: Constraint, actual: OpenApi) extends Violation {
    override def toConstraint: Option[Constraint] = constraint.some
    override def toActual: Option[OpenApi] = actual.some
  }

  final case class Conflict(actual: OpenApi) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[OpenApi] = actual.some
  }

  final case class Invalid(reference: Option[OpenApi], actual: OpenApi) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[OpenApi] = actual.some
  }

  final case class Missing(reference: Option[String]) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[OpenApi] = none
  }

  final case class Unknown(actual: OpenApi) extends Violation {
    override def toConstraint: Option[Constraint] = none
    override def toActual: Option[OpenApi] = actual.some
  }

  def apply[A: Encoder](constraint: Constraint, actual: A): Violation = Validation(constraint, actual.asOpenApi)
  def conflict[A: Encoder](actual: A): Violation = Conflict(actual.asOpenApi)
  def invalid[A: Encoder](reference: A, actual: A): Violation = Invalid(reference.asOpenApi.some, actual.asOpenApi)
  def invalid[A: Encoder](actual: A): Violation = Invalid(reference = none, actual.asOpenApi)
  val missing: Violation = Missing(reference = none)
  def missing(reference: String): Violation = Missing(reference.some)
  def unknown(actual: OpenApi): Violation = Unknown(actual)
}
