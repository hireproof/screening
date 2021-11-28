package io.taig.inspector

import java.time.temporal.Temporal
import scala.util.matching.Regex

sealed abstract class Error extends Product with Serializable

object Error {
  sealed abstract class Collection extends Error

  object Collection {
    final case class AtLeast(reference: Int, actual: Int) extends Collection
    final case class AtMost(reference: Int, actual: Int) extends Collection
    final case class Contains(reference: String, actual: Seq[String]) extends Collection
    final case class Exactly(expected: Int, actual: Int) extends Collection
  }

  sealed abstract class Date extends Error

  object Date {
    final case class After(reference: Temporal, actual: Temporal) extends Date
    final case class Before(reference: Temporal, actual: Temporal) extends Date
  }

  sealed abstract class Enumeration extends Error

  object Enumeration {
    final case class OneOf(references: List[String], actual: String) extends Enumeration
  }

  sealed abstract class Numeric extends Error

  object Numeric {
    final case class Equals(expected: Double, actual: Double) extends Numeric
    final case class GreaterThan(equals: Boolean, expected: Double, actual: Double) extends Numeric
    final case class LessThan(equals: Boolean, expected: Double, actual: Double) extends Numeric
  }

  final case class Parsing(expected: Validation.Attribute.Parsing.Value, actual: String) extends Error

  sealed abstract class Text extends Error

  object Text {
    final case class AtLeast(reference: Int, actual: Int) extends Text
    final case class AtMost(reference: Int, actual: Int) extends Text
    final case class Email(actual: String) extends Text
    final case class Equal(expected: String, actual: String) extends Text
    final case class Exactly(expected: Int, actual: Int) extends Text
    final case class Matches(regex: Regex, actual: String) extends Text
  }
}
