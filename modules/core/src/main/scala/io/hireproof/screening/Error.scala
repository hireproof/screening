package io.hireproof.screening

sealed abstract class Error extends Product with Serializable {
  def constraints: Set[Constraint]
}

object Error {
  final case class BrokenConstraint(constraint: Constraint, actual: Any) extends Error {
    override def constraints: Set[Constraint] = Set(constraint)
  }

  def fromAny(constraint: Constraint, actual: Any): Error = BrokenConstraint(constraint, actual)
}
