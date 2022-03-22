package io.hireproof.screening

import cats.Show
import cats.syntax.all._

sealed abstract class Value extends Product with Serializable {
  override def toString: String = this match {
    case Value.VBoolean(value) => String.valueOf(value)
    case Value.VString(value) => value
    case Value.VList(values) => values.mkString("[", ", ", "]")
    case number: Number => number.toString
  }
}

object Value {
  final private[screening] case class VBoolean(value: Boolean) extends Value
  final private[screening] case class VString(value: String) extends Value
  final private[screening] case class VList(values: List[Value]) extends Value

  def fromBoolean(value: Boolean): Value = VBoolean(value)
  def fromList(value: List[Value]): Value = VList(value)
  def fromShow[A: Show](value: A): Value = fromString(value.show)
  def fromString(value: String): Value = VString(value)
}

sealed abstract class Number extends Value {
  override def toString: String = this match {
    case Number.VDouble(value) => String.valueOf(value)
    case Number.VFloat(value) => String.valueOf(value)
    case Number.VLong(value) => String.valueOf(value)
    case Number.VBigDecimal(value) => String.valueOf(value)
    case Number.VBigInt(value) => String.valueOf(value)
  }
}

object Number {
  final private[screening] case class VDouble(value: Double) extends Number
  final private[screening] case class VFloat(value: Float) extends Number
  final private[screening] case class VInt(value: Int) extends Number
  final private[screening] case class VLong(value: Long) extends Number
  final private[screening] case class VBigDecimal(value: BigDecimal) extends Number
  final private[screening] case class VBigInt(value: BigInt) extends Number

  def fromBigDecimal(value: BigDecimal): Number = Number.VBigDecimal(value)
  def fromBigInt(value: BigInt): Number = Number.VBigInt(value)
  def fromDouble(value: Double): Number = Number.VDouble(value)
  def fromFloat(value: Float): Number = Number.VFloat(value)
  def fromInt(value: Int): Number = Number.VInt(value)
  def fromLong(value: Long): Number = Number.VLong(value)
}