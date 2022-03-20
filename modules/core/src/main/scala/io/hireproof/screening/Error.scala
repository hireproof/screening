package io.hireproof.screening

sealed abstract class Error extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Error {
  final case class BrokenConstraint(constraint: Constraint, actual: Any) extends Error {
    override def constraints: Set[Constraint] = Set(constraint)
  }

  final case class Conflict(actual: String) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Invalid(reference: Option[String], actual: String) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  final case class Missing(reference: Option[String]) extends Error {
    override def constraints: Set[Constraint] = Set.empty
  }

  def fromAny(constraint: Constraint, actual: Any): Error = BrokenConstraint(constraint, actual)
}
