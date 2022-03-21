package io.hireproof.screening

import cats.Show
import cats.syntax.all._

import scala.Numeric.Implicits._

final case class Reference(value: String) extends AnyVal

object Reference {
  def fromShow[A: Show](value: A): Reference = Reference(value.show)

  def fromNumeric[A: Numeric](value: A): Reference = fromShow(value.toDouble)

  def fromIterable[A: Show](values: Iterable[A]): Reference = Reference(values.map(_.show).mkString("[", ", ", "]"))
}
