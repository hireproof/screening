package io.hireproof

import cats.data.ValidatedNel
import cats.syntax.all._

package object screening {
  val hist: Selection.History = Selection.History.Root

  implicit class RichValidatedNel[E, A](val validated: ValidatedNel[E, A]) extends AnyVal {
    def errors: List[E] = validated.fold(_.toList, _ => Nil)

    def error: Option[E] = validated.fold(_.head.some, _ => none)
  }
}
