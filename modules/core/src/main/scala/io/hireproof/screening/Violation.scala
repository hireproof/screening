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

  def apply(constraint: Constraint, actual: String): Violation = Validation(constraint, Actual(actual))
  def conflict(actual: String): Violation = Conflict(Actual(actual))
  def invalid(reference: String, actual: String): Violation = Invalid(Reference(reference).some, Actual(actual))
  def invalid(actual: String): Violation = Invalid(reference = none, Actual(actual))
  val missing: Violation = Missing(reference = none)
  def missing(reference: String): Violation = Missing(Reference(reference).some)
  def unknown(actual: String): Violation = Unknown(Actual(actual))
}
