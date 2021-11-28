package io.taig.inspector

import io.taig.inspector.Validations.Errors

import java.time.temporal.Temporal
import scala.util.matching.Regex

trait ErrorValidations extends Validations[Error] {
  override val errors: Validations.Errors[Error] = new ErrorValidations.Errors {}
}

object ErrorValidations extends ErrorValidations {
  trait Errors extends Validations.Errors[Error] {
    override val collection: Errors.Collection[Error.Collection] = new Errors.Collection[Error.Collection] {
      override def atLeast(expected: Int, actual: Int): Error.Collection = Error.Collection.AtLeast(expected, actual)

      override def atMost(expected: Int, actual: Int): Error.Collection = Error.Collection.AtMost(expected, actual)

      override def contains(reference: String, actual: Seq[String]): Error.Collection =
        Error.Collection.Contains(reference, actual)

      override def exactly(expected: Int, actual: Int): Error.Collection = Error.Collection.Exactly(expected, actual)
    }

    override val date: Errors.Date[Error.Date] = new Errors.Date[Error.Date] {
      override def after(reference: Temporal, actual: Temporal): Error.Date = Error.Date.After(reference, actual)

      override def before(reference: Temporal, actual: Temporal): Error.Date = Error.Date.Before(reference, actual)
    }

    override val enumeration: Errors.Enumeration[Error.Enumeration] = new Errors.Enumeration[Error.Enumeration] {
      override def oneOf(references: List[String], actual: String): Error.Enumeration =
        Error.Enumeration.OneOf(references, actual)
    }

    override val numeric: Errors.Numeric[Error.Numeric] = new Errors.Numeric[Error.Numeric] {
      override def equal(expected: Double, actual: Double): Error.Numeric = Error.Numeric.Equals(expected, actual)

      override def greaterThan(equals: Boolean, expected: Double, actual: Double): Error.Numeric =
        Error.Numeric.GreaterThan(equals, expected, actual)

      override def lessThan(equals: Boolean, expected: Double, actual: Double): Error.Numeric =
        Error.Numeric.LessThan(equals, expected, actual)
    }

    override val parsing: Errors.Parsing[Error] = new Errors.Parsing[Error.Parsing] {
      override def apply(value: Validation.Attribute.Parsing.Value, actual: String): Error.Parsing =
        Error.Parsing(value, actual)
    }

    override val text: Errors.Text[Error.Text] = new Errors.Text[Error.Text] {
      override def atLeast(reference: Int, actual: Int): Error.Text = Error.Text.AtLeast(reference, actual)

      override def atMost(reference: Int, actual: Int): Error.Text = Error.Text.AtMost(reference, actual)

      override def email(actual: String): Error.Text = Error.Text.Email(actual)

      override def equal(expected: String, actual: String): Error.Text = Error.Text.Equal(expected, actual)

      override def exactly(expected: Int, actual: Int): Error.Text = Error.Text.Exactly(expected, actual)

      override def matches(regex: Regex, actual: String): Error.Text = Error.Text.Matches(regex, actual)
    }
  }
}
