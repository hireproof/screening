package io.hireproof.screening

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Encoder, Json}

sealed abstract class Violation extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Violation {
  final case class Validation(constraint: Constraint, actual: Json) extends Violation {
    override def constraints: Set[Constraint] = Set(constraint)
  }

  final case class Conflict(actual: Json) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Invalid(reference: Option[Json], actual: Json) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Missing(reference: Option[Json]) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Unknown(actual: Json) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  def apply[A: Encoder](constraint: Constraint, actual: A): Violation = Validation(constraint, actual.asJson)
  def conflict[A: Encoder](actual: A): Violation = Conflict(actual.asJson)
  def invalid[A: Encoder](reference: A, actual: A): Violation = Invalid(reference.asJson.some, actual.asJson)
  def invalid[A: Encoder](actual: A): Violation = Invalid(reference = none, actual.asJson)
  val missing: Violation = Missing(reference = none)
  def missing[A: Encoder](reference: A): Violation = Missing(reference.asJson.some)
  def unknown[A: Encoder](actual: A): Violation = Unknown(actual.asJson)
}
