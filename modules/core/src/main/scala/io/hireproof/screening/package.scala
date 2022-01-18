package io.hireproof

import cats.data.ValidatedNel
import cats.syntax.all._

import java.time.Instant
import java.time.format.DateTimeParseException
import java.util.UUID

package object screening {
  val __ : Selection.History = Selection.History.Root

  implicit class RichValidatedNel[E, A](val validated: ValidatedNel[E, A]) extends AnyVal {
    def errors: List[E] = validated.fold(_.toList, _ => Nil)

    def error: Option[E] = validated.fold(_.head.some, _ => none)
  }

  private[screening] def parseBigDecimal(value: String): Option[BigDecimal] =
    try { BigDecimal(value).some }
    catch { case _: NumberFormatException => none }

  private[screening] def parseBigInt(value: String): Option[BigInt] =
    try { BigInt(value).some }
    catch { case _: NumberFormatException => none }

  private[screening] def parseInstant(value: String): Option[Instant] =
    try { Instant.parse(value).some }
    catch { case _: DateTimeParseException => none }

  private[screening] def parseUuid(value: String): Option[UUID] =
    try { UUID.fromString(value).some }
    catch { case _: IllegalArgumentException => none }
}
