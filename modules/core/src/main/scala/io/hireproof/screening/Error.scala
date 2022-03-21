package io.hireproof.screening

sealed abstract class Error extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Error {
  final case class BrokenConstraint(constraint: Constraint, actual: Actual) extends Error {
    override def constraints: Set[Constraint] = Set(constraint)
  }

  final case class Conflict(actual: Actual) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Invalid(reference: Option[Reference], actual: Actual) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Missing(reference: Option[Reference]) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Unknown(actual: Actual) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  def apply(constraint: Constraint, actual: Any): Error = BrokenConstraint(constraint, Actual.fromAny(actual))
}
