package io.hireproof.screening

sealed abstract class Violation extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Violation {
  final case class BrokenConstraint(constraint: Constraint, actual: Actual) extends Violation {
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

  def apply(constraint: Constraint, actual: Any): Violation = BrokenConstraint(constraint, Actual.fromAny(actual))
}
