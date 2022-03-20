package io.hireproof.screening

import munit.FunSuite
import io.hireproof.screening.validations._
import cats.syntax.all._

import java.time._

final class ValidationTest extends FunSuite {
  test("collection.atLeast") {
    assert(list.atLeast(reference = 1).run(List(1, 2, 3)).isValid)
    assert(list.atLeast(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = list.atLeast(reference = 3).run(List(1)).error,
      expected = Constraint.Collection.AtLeast(equal = true, reference = 3).some
    )
  }

  test("collection.atMost") {
    assert(list.atMost(reference = 3).run(List(1)).isValid)
    assert(list.atMost(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = list.atMost(reference = 1).run(List(1, 2, 3)).error,
      expected = Some(Constraint.Collection.AtMost(equal = true, reference = 1))
    )
  }

//  test("collection.isEmpty") {
//    assert(foldable.empty[List, Int].run(Nil).isValid)
//    assertEquals(
//      obtained = foldable.empty[List, Int].run(List(1, 2, 3)).error,
//      expected = Some(Validation.Error.Collection.AtMost(equal = true, reference = 0, actual = 3))
//    )
//  }
//
//  test("collection.nonEmpty") {
//    assert(foldable.nonEmpty[List, Int].run(List(1, 2, 3)).isValid)
//    assertEquals(
//      obtained = foldable.nonEmpty[List, Int].run(Nil).error,
//      expected = Some(Validation.Error.Collection.AtLeast(equal = false, reference = 0, actual = 0))
//    )
//  }
//
//  test("collection.exactly") {
//    assert(foldable.exactly[List, Int](expected = 3).run(List(1, 2, 3)).isValid)
//    assert(foldable.exactly[List, Int](expected = 0).run(Nil).isValid)
//    assertEquals(
//      obtained = foldable.exactly[List, Int](expected = 3).run(List(1)).error,
//      expected = Some(Validation.Error.Collection.Exactly(reference = 3, actual = 1))
//    )
//  }
//
//  test("collection.contains") {
//    assert(foldable.contains[List, String](reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
//    assertEquals(
//      obtained = foldable.contains[List, String](reference = "foobar").run(List("foo", "bar")).error,
//      expected = Some(Validation.Error.Collection.Contains(reference = "foobar", actual = List("foo", "bar")))
//    )
//  }
//
//  test("date.after: Instant") {
//    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)
//
//    assert(date.after(sample, equal = true).run(sample.plusSeconds(100)).isValid)
//    assertEquals(
//      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
//      expected = Some(Validation.Error.Date.After(equal = true, sample, sample.minusSeconds(100)))
//    )
//  }
//
//  test("date.before: Instant") {
//    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)
//
//    assert(date.before(sample, equal = true).run(sample.minusSeconds(100)).isValid)
//    assertEquals(
//      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
//      expected = Some(Validation.Error.Date.Before(equal = true, sample, sample.plusSeconds(100)))
//    )
//  }
//
//  test("number.greaterThan (equal: false)") {
//    assert(number.greaterThan(1, equal = false).run(3).isValid)
//    assertEquals(
//      obtained = number.greaterThan(3, equal = false).run(3).error,
//      expected = Some(Validation.Error.Number.GreaterThan(equal = false, 3, 3))
//    )
//    assertEquals(
//      obtained = number.greaterThan(3, equal = false).run(1).error,
//      expected = Some(Validation.Error.Number.GreaterThan(equal = false, 3, 1))
//    )
//  }
//
//  test("number.greaterThan (equal: true)") {
//    assert(number.greaterThan(1, equal = true).run(3).isValid)
//    assert(number.greaterThan(3, equal = true).run(3).isValid)
//
//    assertEquals(
//      obtained = number.greaterThan(3, equal = true).run(1).error,
//      expected = Some(Validation.Error.Number.GreaterThan(equal = true, 3, 1))
//    )
//  }
//
//  test("number.lessThan (equal: false)") {
//    assert(number.lessThan(3, equal = false).run(1).isValid)
//    assertEquals(
//      obtained = number.lessThan(3, equal = false).run(3).error,
//      expected = Some(Validation.Error.Number.LessThan(equal = false, 3, 3))
//    )
//
//    assertEquals(
//      obtained = number.lessThan(1, equal = false).run(3).error,
//      expected = Some(Validation.Error.Number.LessThan(equal = false, 1, 3))
//    )
//  }
//
//  test("number.lessThan (equal: true)") {
//    assert(number.lessThan(3, equal = true).run(1).isValid)
//    assert(number.lessThan(3, equal = true).run(3).isValid)
//
//    assertEquals(
//      obtained = number.lessThan(1, equal = true).run(3).error,
//      expected = Some(Validation.Error.Number.LessThan(equal = true, 1, 3))
//    )
//  }
//
//  test("number.equal") {
//    assert(number.equal(3).run(3).isValid)
//    assert(number.equal(3f).run(3f).isValid)
//    assert(number.equal(3d).run(3d).isValid)
//
//    assertEquals(
//      obtained = number.equal(1).run(3).error,
//      expected = Some(Validation.Error.Number.Equal(1, 3))
//    )
//    assertEquals(
//      obtained = number.equal(0.3d).run(0.1d + 0.2d).error,
//      expected = Some(Validation.Error.Number.Equal(reference = 0.3d, actual = 0.1d + 0.2d))
//    )
//
//    assert(number.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.2d).isValid)
//  }
//
//  test("parsing.bigDecimal") {
//    assert(parsing.bigDecimal.run("3.14").isValid)
//    assert(parsing.bigDecimal.run("3").isValid)
//    assertEquals(
//      obtained = parsing.bigDecimal.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("BigDecimal", "foobar"))
//    )
//  }
//
//  test("parsing.bigInt") {
//    assert(parsing.bigInt.run("3").isValid)
//    assert(parsing.bigInt.run("0").isValid)
//    assertEquals(
//      obtained = parsing.bigInt.run("3.14").error,
//      expected = Some(Validation.Error.Parsing("BigInt", "3.14"))
//    )
//    assertEquals(
//      obtained = parsing.bigInt.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("BigInt", "foobar"))
//    )
//  }
//
//  test("parsing.double") {
//    assert(parsing.double.run("3.14").isValid)
//    assert(parsing.double.run("3").isValid)
//    assertEquals(
//      obtained = parsing.double.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("Double", "foobar"))
//    )
//  }
//
//  test("parsing.float") {
//    assert(parsing.float.run("3.14").isValid)
//    assert(parsing.float.run("3").isValid)
//    assertEquals(
//      obtained = parsing.float.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("Float", "foobar"))
//    )
//  }
//
//  test("parsing.int") {
//    assert(parsing.int.run("3").isValid)
//    assert(parsing.int.run("0").isValid)
//    assertEquals(
//      obtained = parsing.int.run("3.14").error,
//      expected = Some(Validation.Error.Parsing("Int", "3.14"))
//    )
//    assertEquals(
//      obtained = parsing.int.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("Int", "foobar"))
//    )
//  }
//
//  test("parsing.long") {
//    assert(parsing.long.run("3").isValid)
//    assert(parsing.long.run("0").isValid)
//    assertEquals(
//      obtained = parsing.long.run("3.14").error,
//      expected = Some(Validation.Error.Parsing("Long", "3.14"))
//    )
//    assertEquals(
//      obtained = parsing.long.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("Long", "foobar"))
//    )
//  }
//
//  test("parsing.short") {
//    assert(parsing.short.run("3").isValid)
//    assert(parsing.short.run("0").isValid)
//    assertEquals(
//      obtained = parsing.short.run("3.14").error,
//      expected = Some(Validation.Error.Parsing("Short", "3.14"))
//    )
//    assertEquals(
//      obtained = parsing.short.run("foobar").error,
//      expected = Some(Validation.Error.Parsing("Short", "foobar"))
//    )
//  }
//
//  test("text.atLeast") {
//    assert(text.atLeast(reference = 1, equal = true).run("foo").isValid)
//    assert(text.atLeast(reference = 3, equal = true).run("foo").isValid)
//    assertEquals(
//      obtained = text.atLeast(reference = 3, equal = true).run("fo").error,
//      expected = Some(Validation.Error.Text.AtLeast(equal = true, reference = 3, actual = 2))
//    )
//  }
//
//  test("text.atMost") {
//    assert(text.atMost(reference = 3, equal = true).run("fo").isValid)
//    assert(text.atMost(reference = 3, equal = true).run("foo").isValid)
//    assertEquals(
//      obtained = text.atMost(reference = 1, equal = true).run("foo").error,
//      expected = Some(Validation.Error.Text.AtMost(equal = true, reference = 1, actual = 3))
//    )
//  }
//
//  test("text.isEmpty") {
//    assert(text.empty.run("").isValid)
//    assertEquals(
//      obtained = text.empty.run("foo").error,
//      expected = Some(Validation.Error.Text.AtMost(equal = true, reference = 0, actual = 3))
//    )
//  }
//
//  test("text.nonEmpty") {
//    assert(text.nonEmpty.run("foobar").isValid)
//    assertEquals(
//      obtained = text.nonEmpty.run("").error,
//      expected = Some(Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0))
//    )
//  }
//
//  test("text.exactly") {
//    assert(text.exactly(expected = 3).run("foo").isValid)
//    assert(text.exactly(expected = 0).run("").isValid)
//    assertEquals(
//      obtained = text.exactly(expected = 1).run("foo").error,
//      expected = Some(Validation.Error.Text.Exactly(reference = 1, actual = 3))
//    )
//  }
//
//  test("text.matches") {
//    val Whitespace = "\\s+".r
//
//    assert(text.matches(regex = Whitespace).run("   ").isValid)
//    assertEquals(
//      obtained = text.matches(regex = Whitespace).run(" foobar ").error,
//      expected = Some(Validation.Error.Text.Matches(regex = Whitespace, " foobar "))
//    )
//    assertEquals(
//      obtained = text.matches(regex = Whitespace).run("").error,
//      expected = Some(Validation.Error.Text.Matches(regex = Whitespace, ""))
//    )
//  }
//
//  test("text.email") {
//    assert(text.email.run("mail@taig.io").isValid)
//    assertEquals(
//      obtained = text.email.run("foobar").error,
//      expected = Some(Validation.Error.Text.Email("foobar"))
//    )
//  }
//
//  test("toDebugString") {
//    val validation = text.required
//      .andThen(text.atLeast(reference = 3, equal = false) and text.atMost(reference = 10, equal = true))
//
//    assertEquals(
//      obtained = validation.toDebugString,
//      expected = "(a.length > 0).andThen(b => (b.length > 3) && (b.length <= 10))"
//    )
//  }
}
