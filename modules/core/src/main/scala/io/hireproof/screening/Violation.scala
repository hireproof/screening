package io.hireproof.screening

import cats.syntax.all._

sealed abstract class Violation extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Violation {
  final case class Validation(constraint: Constraint, actual: Actual) extends Violation {
    override def constraints: Set[Constraint] = Set(constraint)
  }

  final case class Conflict(actual: Actual) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Invalid(reference: Option[Reference], actual: Actual) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Missing(reference: Option[Reference]) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Unknown(actual: Actual) extends Violation {
    override def constraints: Set[Constraint] = Set.empty
  }

  def apply(constraint: Constraint, actual: Any): Violation = Validation(constraint, Actual.fromAny(actual))
  def conflict(actual: Any): Violation = Conflict(Actual.fromAny(actual))
  def invalid(reference: Any, actual: Any): Violation =
    Invalid(Reference.fromAny(reference).some, Actual.fromAny(actual))
  def invalid(actual: Any): Violation = Invalid(reference = none, Actual.fromAny(actual))
  val missing: Violation = Missing(reference = none)
  def missing(reference: Any): Violation = Missing(Reference.fromAny(reference).some)
  def unknown(actual: Any): Violation = Unknown(Actual.fromAny(actual))
}
