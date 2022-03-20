package io.hireproof.screening

import java.time.ZonedDateTime
import scala.concurrent.duration.FiniteDuration
import scala.util.matching.Regex

sealed abstract class Constraint extends Product with Serializable {
  def toDebugString: String
}

object Constraint {
  final case class Or(left: Set[Constraint], right: Set[Constraint]) extends Constraint {
    override def toDebugString: String =
      s"(${left.map(_.toDebugString).mkString(" && ")}) || (${right.map(_.toDebugString).mkString(" && ")})"
  }

  sealed abstract class Collection extends Constraint

  object Collection {
    final case class AtLeast(equal: Boolean, reference: Long) extends Collection {
      override def toDebugString: String = if (equal) s"_.length >= $reference" else s"_.length > $reference"
    }

    final case class AtMost(equal: Boolean, reference: Long) extends Collection {
      override def toDebugString: String = if (equal) s"_.length <= $reference" else s"_.length < $reference"
    }

    final case class Exactly(reference: Long) extends Collection {
      override def toDebugString: String = s"_.length == $reference"
    }

    final case class Contains(reference: String) extends Collection {
      override def toDebugString: String = s"_.contains($reference)"
    }
  }

  sealed abstract class Duration extends Constraint

  object Duration {
    final case class AtLeast(equal: Boolean, reference: FiniteDuration) extends Duration {
      override def toDebugString: String = if (equal) s"_ >= $reference" else s"_ > $reference"
    }

    final case class AtMost(equal: Boolean, reference: FiniteDuration) extends Duration {
      override def toDebugString: String = if (equal) s"_ <= $reference" else s"_ < $reference"
    }

    final case class Exactly(reference: FiniteDuration) extends Duration {
      override def toDebugString: String = s"_ == $reference"
    }
  }

  sealed abstract class Number extends Constraint

  object Number {
    final case class Equal(reference: Double, delta: Double) extends Number {
      override def toDebugString: String = s"_ == $reference"
    }

    final case class GreaterThan(equal: Boolean, reference: Double, delta: Double) extends Number {
      override def toDebugString: String = if (equal) s"_ >= $reference" else s"_ > $reference"
    }

    final case class LessThan(equal: Boolean, reference: Double, delta: Double) extends Number {
      override def toDebugString: String = if (equal) s"_ <= $reference" else s"_ < $reference"
    }
  }

  sealed abstract class Optional extends Constraint

  object Optional {
    final case object IsDefined extends Constraint {
      override def toDebugString: String = "_.isDefined"
    }
  }

  final case class Parsing(reference: String) extends Constraint {
    override def toDebugString: String = s"_.parse($reference)"
  }

  sealed abstract class Text extends Constraint

  object Text {
    final case class AtLeast(equal: Boolean, reference: Int) extends Text {
      override def toDebugString: String = if (equal) s"_.length >= $reference" else s"_.length > $reference"
    }

    final case class AtMost(equal: Boolean, reference: Int) extends Text {
      override def toDebugString: String = if (equal) s"_.length <= $reference" else s"_.length < $reference"
    }

    final case class Equal(reference: String) extends Text {
      override def toDebugString: String = s"_ == $reference"
    }

    final case class Exactly(reference: Int) extends Text {
      override def toDebugString: String = s"_.length == $reference"
    }

    final case class Matches(regex: Regex) extends Text {
      override def toDebugString: String = s"_.matches(${regex.regex})"
    }
  }

  sealed abstract class Time extends Constraint

  object Time {
    final case class After(equal: Boolean, reference: ZonedDateTime) extends Time {
      override def toDebugString: String = s"_.isAfter($reference)"
    }

    final case class Before(equal: Boolean, reference: ZonedDateTime) extends Time {
      override def toDebugString: String = s"_.isBefore($reference)"
    }

  }
}
