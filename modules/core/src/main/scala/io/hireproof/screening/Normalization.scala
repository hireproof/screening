package io.hireproof.screening

import cats.syntax.all._

object Normalization {
  def apply[I, O](validation: Validation[I, O]): Option[Validation[I, O]] = validation match {
    case Validation.Not(Validation.Not(validation)) => Normalization[I, Unit](validation)
    case Validation.Not(validation: Validation[I, Unit]) =>
      not[I](validation).orElse(Normalization[I, Unit](validation).map(Validation.Not.apply))
    case validation: Validation.And[_]           => and(validation)
    case validation: Validation.AndThen[_, _, _] => andThen(validation)
    case validation: Validation.First[_, _, _]   => first(validation)
    case validation: Validation.Modify[_, _]     => modify(validation)
    case validation: Validation.Or[_, _]         => or(validation)
    case _                                       => none
  }

  private def not[I]: Validation[I, Unit] => Option[Validation[I, Unit]] = {
    case validation: Validation.Collection.AtMost[_, _]  => validation.negate.some
    case validation: Validation.Collection.AtLeast[_, _] => validation.negate.some
    case Validation.Text.AtMost(equal, reference)        => Validation.Text.AtLeast(!equal, reference).some
    case Validation.Text.AtLeast(equal, reference)       => Validation.Text.AtMost(!equal, reference).some
    case _                                               => none
  }

  private def and[I]: Validation.And[I] => Option[Validation.And[I]] = and =>
    (Normalization(and.left), Normalization(and.right)) match {
      case (Some(left), Some(right)) => Validation.And(left, right).some
      case (Some(left), None)        => Validation.And(left, and.right).some
      case (None, Some(right))       => Validation.And(and.left, right).some
      case (None, None)              => None
    }

  private def andThen[I, X, O]: Validation.AndThen[I, X, O] => Option[Validation.AndThen[I, X, O]] = andThen =>
    (Normalization(andThen.left), Normalization(andThen.right)) match {
      case (Some(left), Some(right)) => Validation.AndThen(left, right).some
      case (Some(left), None)        => Validation.AndThen(left, andThen.right).some
      case (None, Some(right))       => Validation.AndThen(andThen.left, right).some
      case (None, None)              => None
    }

  private def first[I, X, O]: Validation.First[I, X, O] => Option[Validation.First[I, X, O]] = first =>
    Normalization(first.validation).map(Validation.First.apply)

  private def modify[I, O]: Validation.Modify[I, O] => Option[Validation.Modify[I, O]] = modify =>
    Normalization(modify.validation).map(Validation.Modify(_, modify.f))

  private def or[I, O]: Validation.Or[I, O] => Option[Validation.Or[I, O]] = or =>
    (Normalization(or.left), Normalization(or.right)) match {
      case (Some(left), Some(right)) => Validation.Or(left, right).some
      case (Some(left), None)        => Validation.Or(left, or.right).some
      case (None, Some(right))       => Validation.Or(or.left, right).some
      case (None, None)              => None
    }
}
