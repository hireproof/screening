package io.hireproof.screening

import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

sealed abstract class Constraint extends Product with Serializable

object Constraint {
  final case class Not(constraint: Set[Constraint]) extends Constraint
  final case class Or(left: Set[Constraint], right: Set[Constraint]) extends Constraint

  sealed abstract class Collection extends Constraint

  object Collection {
    final case class AtLeast(equal: Boolean, reference: Long) extends Collection
    final case class AtMost(equal: Boolean, reference: Long) extends Collection
    final case class Contains(reference: String) extends Collection
  }

  sealed abstract class Duration extends Constraint

  object Duration {
    final case class AtLeast(equal: Boolean, reference: FiniteDuration) extends Duration
    final case class AtMost(equal: Boolean, reference: FiniteDuration) extends Duration
  }

  sealed abstract class Number extends Constraint

  object Number {
    final case class GreaterThan(equal: Boolean, reference: Double, delta: Double) extends Number
    final case class LessThan(equal: Boolean, reference: Double, delta: Double) extends Number
  }

  sealed abstract class Optional extends Constraint

  object Optional {
    final case object IsDefined extends Constraint
  }

  final case class Parsing(reference: String) extends Constraint

  sealed abstract class Text extends Constraint

  object Text {
    final case class AtLeast(equal: Boolean, reference: Int) extends Text
    final case class AtMost(equal: Boolean, reference: Int) extends Text
    final case class Equal(reference: String) extends Text
    final case class Exactly(reference: Int) extends Text
    final case class Matches(regex: Regex) extends Text
  }

  sealed abstract class Time extends Constraint

  object Time {
    final case class After(equal: Boolean, reference: ZonedDateTime) extends Time
    final case class Before(equal: Boolean, reference: ZonedDateTime) extends Time
  }
}
