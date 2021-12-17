package io.taig.inspector

import cats.data.Validated
import cats.syntax.all._

abstract class Cursor[A] { self =>
  def get: Validated[ValidationGroup.Error, A]

  def history: Selection.History

  final def run[B](validation: Validation[A, B]): Validated[ValidationGroup.Errors, B] =
    andThen(validation).get.leftMap(error => ValidationGroup.Errors(history -> error))

  final def lift: Validated[ValidationGroup.Errors, A] = get.leftMap(error => ValidationGroup.Errors(history -> error))

  final def map[B](f: A => B): Cursor[B] = new Cursor[B] {
    override def get: Validated[ValidationGroup.Error, B] = self.get.map(f)

    override def history: Selection.History = self.history
  }

  final def field[B](name: String, select: A => B): Cursor[B] = new Cursor[B] {
    override val get: Validated[ValidationGroup.Error, B] = self.get.map(select)

    override def history: Selection.History = name :: self.history
  }

  final def branch[B](name: String, select: PartialFunction[A, B]): Cursor[B] = new Cursor[B] {
    override val get: Validated[ValidationGroup.Error, B] = self.get
      .andThen(select.lift(_).toValid(Left(s"Failed to select branch '$name'")))

    override def history: Selection.History = name :: self.history
  }

  final def andThen[B](validation: Validation[A, B]): Cursor[B] = new Cursor[B] {
    override def get: Validated[ValidationGroup.Error, B] = self.get.andThen(validation.run(_).leftMap(_.asRight))

    override def history: Selection.History = self.history
  }

  final def or(cursor: Cursor[A]): Cursor[A] = new Cursor[A] {
    val (get, history): (Validated[ValidationGroup.Error, A], Selection.History) = self.get match {
      case valid @ Validated.Valid(_)            => (valid, self.history)
      case invalid @ Validated.Invalid(Right(_)) => (invalid, self.history)
      case Validated.Invalid(Left(_))            => (cursor.get, cursor.history)
    }
  }
}

object Cursor {
  def root[A](value: A): Cursor[A] = new Cursor[A] {
    override val get: Validated[ValidationGroup.Error, A] = Validated.valid(value)

    override val history: Selection.History = Selection.History.Root
  }
}
