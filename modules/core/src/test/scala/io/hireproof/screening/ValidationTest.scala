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
      expected = Error(Constraint.number.greaterThan(reference = 3, equal = true, delta = 0), actual = 1).some
    )
  }

  test("collection.atMost") {
    assert(list.atMost(reference = 3).run(List(1)).isValid)
    assert(list.atMost(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = list.atMost(reference = 1).run(List(1, 2, 3)).error,
      expected = Error(Constraint.number.lessThan(reference = 1, equal = true, delta = 0), actual = 3).some
    )
  }

  test("collection.empty") {
    assert(list.empty.run(Nil).isValid)
    assertEquals(
      obtained = list.empty.run(List(1, 2, 3)).error,
      expected = Error(Constraint.number.lessThan(reference = 0, equal = true, delta = 0), actual = 3).some
    )
  }

  test("collection.nonEmpty") {
    assert(list.nonEmpty.run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = list.nonEmpty.run(Nil).error,
      expected = Error(Constraint.number.greaterThan(reference = 1, equal = true, delta = 0), actual = 0).some
    )
  }

  test("collection.exactly") {
    assert(list.exactly(reference = 3).run(List(1, 2, 3)).isValid)
    assert(list.exactly(reference = 0).run(Nil).isValid)
    assertEquals(
      obtained = list.exactly(reference = 3).run(List(1)).error,
      expected = Error(Constraint.number.equal(reference = 3, delta = 0), actual = 1).some
    )
  }

  test("collection.contains") {
    assert(list.contains(reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
    assertEquals(
      obtained = list.contains(reference = "foobar").run(List("foo", "bar")).error,
      expected = Error(Constraint.collection.contains(reference = "foobar"), actual = List("foo", "bar")).some
    )
  }

  test("time.after: Instant") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assert(time.after(sample, equal = true).run(sample).isValid)
    assert(time.after(sample, equal = true).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = time.after(sample).run(sample.minusSeconds(100)).error,
      expected = Error(
        Constraint.time.after(sample.atZone(ZoneOffset.UTC), equal = true),
        actual = sample.minusSeconds(100)
      ).some
    )
  }

  test("time.before: Instant") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assert(time.before(sample, equal = true).run(sample).isValid)
    assert(time.before(sample, equal = true).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = time.before(sample).run(sample.plusSeconds(100)).error,
      expected = Error(
        Constraint.time.before(sample.atZone(ZoneOffset.UTC), equal = true),
        actual = sample.plusSeconds(100)
      ).some
    )
  }

  test("number.greaterThan (delta)") {
    assert(number.greaterThan(reference = 1d, delta = 0.5d, equal = true).run(0.75d).isValid)
    assert(number.greaterThan(reference = 1d, delta = 0.5d, equal = true).run(0.5d).isValid)
    assertEquals(
      obtained = number.greaterThan(reference = 1d, delta = 0.5d, equal = false).run(0.5d).error,
      expected = Error(Constraint.number.greaterThan(reference = 1d, delta = 0.5d, equal = false), actual = 0.5d).some
    )
    assertEquals(
      obtained = number.greaterThan(reference = 1d, delta = 0.5d, equal = true).run(0.25d).error,
      expected = Error(Constraint.number.greaterThan(reference = 1d, delta = 0.5d, equal = true), actual = 0.25d).some
    )
  }

  test("number.greaterThanNotEqual") {
    assert(number.greaterThanNotEqual(reference = 1).run(3).isValid)
    assertEquals(
      obtained = number.greaterThanNotEqual(reference = 3).run(3).error,
      expected = Error(Constraint.number.greaterThan(reference = 3, delta = 0, equal = false), actual = 3).some
    )
    assertEquals(
      obtained = number.greaterThanNotEqual(3).run(1).error,
      expected = Error(Constraint.number.greaterThan(reference = 3, delta = 0, equal = false), actual = 1).some
    )
  }

  test("number.greaterThanEqual") {
    assert(number.greaterThanEqual(reference = 1).run(3).isValid)
    assert(number.greaterThanEqual(reference = 3).run(3).isValid)
    assertEquals(
      obtained = number.greaterThanEqual(3).run(1).error,
      expected = Error(Constraint.number.greaterThan(reference = 3, delta = 0, equal = true), actual = 1).some
    )
  }

  test("number.lessThanNotEqual") {
    assert(number.lessThanNotEqual(reference = 3).run(1).isValid)
    assertEquals(
      obtained = number.lessThanNotEqual(reference = 3).run(3).error,
      expected = Error(Constraint.number.lessThan(reference = 3, delta = 0, equal = false), actual = 3).some
    )
    assertEquals(
      obtained = number.lessThanNotEqual(1).run(3).error,
      expected = Error(Constraint.number.lessThan(reference = 1, delta = 0, equal = false), actual = 3).some
    )
  }

  test("number.lessThan (delta)") {
    assert(number.lessThan(reference = 1d, delta = 0.5d, equal = true).run(1.25d).isValid)
    assert(number.lessThan(reference = 1d, delta = 0.5d, equal = true).run(1.5d).isValid)
    assertEquals(
      obtained = number.lessThan(reference = 1d, delta = 0.5d, equal = false).run(1.5d).error,
      expected = Error(Constraint.number.lessThan(reference = 1d, delta = 0.5d, equal = false), actual = 1.5d).some
    )
    assertEquals(
      obtained = number.lessThan(reference = 1d, delta = 0.5d, equal = true).run(1.75d).error,
      expected = Error(Constraint.number.lessThan(reference = 1d, delta = 0.5d, equal = true), actual = 1.75d).some
    )
  }

  test("number.lessThanEqual") {
    assert(number.lessThanEqual(reference = 3).run(1).isValid)
    assert(number.lessThanEqual(reference = 3).run(3).isValid)
    assertEquals(
      obtained = number.lessThanEqual(1).run(3).error,
      expected = Error(Constraint.number.lessThan(reference = 1, delta = 0, equal = true), actual = 3).some
    )
  }

  test("number.equal") {
    assert(number.equal(3).run(3).isValid)
    assert(number.equal(3f).run(3f).isValid)
    assert(number.equal(3d).run(3d).isValid)

    assertEquals(
      obtained = number.equal(1).run(3).error,
      expected = Error(Constraint.number.equal(reference = 1, delta = 0), actual = 3).some
    )
    assertEquals(
      obtained = number.equal(0.3d, 0d).run(0.1d + 0.2d).error,
      expected = Error(Constraint.number.equal(reference = 0.3d, delta = 0d), actual = 0.1d + 0.2d).some
    )
  }

  test("number.equal (delta)") {
    assert(number.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.2d).isValid)
    assert(number.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.25d).isInvalid)
    assert(number.equal(reference = 0.3d, delta = 0.01d).run(0.1d + 0.15d).isInvalid)
  }

  test("parsing.bigDecimal") {
    assert(parsing.bigDecimal.run("3.14").isValid)
    assert(parsing.bigDecimal.run("3").isValid)
    assertEquals(
      obtained = parsing.bigDecimal.run("foobar").error,
      expected = Error(Constraint.parsing("bigDecimal"), actual = "foobar").some
    )
  }

  test("parsing.bigInt") {
    assert(parsing.bigInt.run("3").isValid)
    assert(parsing.bigInt.run("0").isValid)
    assertEquals(
      obtained = parsing.bigInt.run("3.14").error,
      expected = Error(Constraint.parsing("bigInt"), actual = "3.14").some
    )
    assertEquals(
      obtained = parsing.bigInt.run("foobar").error,
      expected = Error(Constraint.parsing("bigInt"), actual = "foobar").some
    )
  }

  test("parsing.double") {
    assert(parsing.double.run("3.14").isValid)
    assert(parsing.double.run("3").isValid)
    assertEquals(
      obtained = parsing.double.run("foobar").error,
      expected = Error(Constraint.parsing("double"), actual = "foobar").some
    )
  }

  test("parsing.float") {
    assert(parsing.float.run("3.14").isValid)
    assert(parsing.float.run("3").isValid)
    assertEquals(
      obtained = parsing.float.run("foobar").error,
      expected = Error(Constraint.parsing("float"), actual = "foobar").some
    )
  }

  test("parsing.int") {
    assert(parsing.int.run("3").isValid)
    assert(parsing.int.run("0").isValid)
    assertEquals(
      obtained = parsing.int.run("3.14").error,
      expected = Error(Constraint.parsing("int"), actual = "3.14").some
    )
    assertEquals(
      obtained = parsing.int.run("foobar").error,
      expected = Error(Constraint.parsing("int"), actual = "foobar").some
    )
  }

  test("parsing.long") {
    assert(parsing.long.run("3").isValid)
    assert(parsing.long.run("0").isValid)
    assertEquals(
      obtained = parsing.long.run("3.14").error,
      expected = Error(Constraint.parsing("long"), actual = "3.14").some
    )
    assertEquals(
      obtained = parsing.long.run("foobar").error,
      expected = Error(Constraint.parsing("long"), actual = "foobar").some
    )
  }

  test("parsing.short") {
    assert(parsing.short.run("3").isValid)
    assert(parsing.short.run("0").isValid)
    assertEquals(
      obtained = parsing.short.run("3.14").error,
      expected = Error(Constraint.parsing("short"), actual = "3.14").some
    )
    assertEquals(
      obtained = parsing.short.run("foobar").error,
      expected = Error(Constraint.parsing("short"), actual = "foobar").some
    )
  }

  test("text.atLeast") {
    assert(text.atLeast(reference = 1, equal = true).run("foo").isValid)
    assert(text.atLeast(reference = 3, equal = true).run("foo").isValid)
    assertEquals(
      obtained = text.atLeast(reference = 3, equal = true).run("fo").error,
      expected = Error(Constraint.number.greaterThan(reference = 3, delta = 0, equal = true), actual = 2).some
    )
  }

  test("text.atMost") {
    assert(text.atMost(reference = 3, equal = true).run("fo").isValid)
    assert(text.atMost(reference = 3, equal = true).run("foo").isValid)
    assertEquals(
      obtained = text.atMost(reference = 1, equal = true).run("foo").error,
      expected = Error(Constraint.number.lessThan(reference = 1, delta = 0, equal = true), actual = 3).some
    )
  }

  test("text.empty") {
    assert(text.empty.run("").isValid)
    assertEquals(
      obtained = text.empty.run("foo").error,
      expected = Error(Constraint.number.lessThan(reference = 0, delta = 0, equal = true), actual = 3).some
    )
  }

  test("text.nonEmpty") {
    assert(text.nonEmpty.run("foobar").isValid)
    assertEquals(
      obtained = text.nonEmpty.run("").error,
      expected = Error(Constraint.number.greaterThan(reference = 1, delta = 0, equal = true), actual = 0).some
    )
  }

  test("text.exactly") {
    assert(text.exactly(reference = 3).run("foo").isValid)
    assert(text.exactly(reference = 0).run("").isValid)
    assertEquals(
      obtained = text.exactly(reference = 1).run("foo").error,
      expected = Error(Constraint.number.equal(reference = 1, delta = 0), actual = 3).some
    )
  }

  test("text.matches") {
    val Whitespace = "\\s+".r

    assert(text.matches(regex = Whitespace).run("   ").isValid)
    assertEquals(
      obtained = text.matches(regex = Whitespace).run(" foobar ").error,
      expected = Error(Constraint.text.matches(regex = Whitespace), actual = " foobar ").some
    )
    assertEquals(
      obtained = text.matches(regex = Whitespace).run("").error,
      expected = Error(Constraint.text.matches(regex = Whitespace), actual = "").some
    )
  }

  test("toDebugString") {
    val validation = text.required
      .andThen(text.atLeast(reference = 3, equal = false) and text.atMost(reference = 10, equal = true))

    assertEquals(
      obtained = validation.toDebugString,
      expected =
        "[greaterThan(reference=1, delta=0, equal=true), greaterThan(reference=3, delta=0, equal=false), lessThan(reference=10, delta=0, equal=true)]"
    )
  }
}
