package io.taig

import cats.data.ValidatedNel
import cats.syntax.all._

package object inspector {
  implicit class RichValidatedNel[E, A](val validated: ValidatedNel[E, A]) extends AnyVal {
    def errors: List[E] = validated.fold(_.toList, _ => Nil)

    def error: Option[E] = validated.fold(_.head.some, _ => none)
  }

  private[inspector] def parseBigDecimal(value: String): Option[BigDecimal] =
    try { BigDecimal(value).some }
    catch { case _: NumberFormatException => none }

  private[inspector] def parseBigInt(value: String): Option[BigInt] =
    try { BigInt(value).some }
    catch { case _: NumberFormatException => none }
}
