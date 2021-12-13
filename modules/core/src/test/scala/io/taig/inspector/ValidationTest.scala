package io.taig.inspector

import munit.FunSuite
import io.taig.inspector.rule._

import java.time._

final class ValidationTest extends FunSuite {
  test("collection.atLeast") {
    assert(collection.atLeast(reference = 1).run(List(1, 2, 3)).isValid)
    assert(collection.atLeast(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.atLeast(reference = 3).run(List(1)).error,
      expected = Some(Validation.Error.Collection.AtLeast(reference = 3, actual = 1))
    )
  }

  test("collection.atMost") {
    assert(collection.atMost(reference = 3).run(List(1)).isValid)
    assert(collection.atMost(reference = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.atMost(reference = 1).run(List(1, 2, 3)).error,
      expected = Some(Validation.Error.Collection.AtMost(reference = 1, actual = 3))
    )
  }

  test("collection.isEmpty") {
    assert(collection.empty.run(Nil).isValid)
    assertEquals(
      obtained = collection.empty.run(List(1, 2, 3)).error,
      expected = Some(Validation.Error.Collection.AtMost(reference = 0, actual = 3))
    )
  }

  test("collection.nonEmpty") {
    assert(collection.nonEmpty.run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = collection.nonEmpty.run(Nil).error,
      expected = Some(Validation.Error.Not(Validation.Error.Collection.AtMost(reference = 0, actual = 0)))
    )
  }

  test("collection.exactly") {
    assert(collection.exactly(expected = 3).run(List(1, 2, 3)).isValid)
    assert(collection.exactly(expected = 0).run(Nil).isValid)
    assertEquals(
      obtained = collection.exactly(expected = 3).run(List(1)).error,
      expected = Some(Validation.Error.Collection.Exactly(expected = 3, actual = 1))
    )
  }

  test("collection.contains") {
    assert(collection.contains(reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
    assertEquals(
      obtained = collection.contains(reference = "foobar").run(List("foo", "bar")).error,
      expected = Some(Validation.Error.Collection.Contains(reference = "foobar", actual = List("foo", "bar")))
    )
  }

  test("date.after: OffsetDateTime") {
    val sample = OffsetDateTime.of(LocalDateTime.of(2021, 11, 29, 12, 30), ZoneOffset.UTC)

    assert(date.after(sample).run(sample.plusDays(1)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusDays(1)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusDays(1)))
    )
  }

  test("date.before: OffsetDateTime") {
    val sample = OffsetDateTime.of(LocalDateTime.of(2021, 11, 29, 12, 30), ZoneOffset.UTC)

    assert(date.before(sample).run(sample.minusDays(1)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusDays(1)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusDays(1)))
    )
  }

  test("date.after: Instant") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assert(date.after(sample).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusSeconds(100)))
    )
  }

  test("date.before: Instant") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assert(date.before(sample).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusSeconds(100)))
    )
  }

  test("date.after: LocalDateTime") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30)

    assert(date.after(sample).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusSeconds(100)))
    )
  }

  test("date.before: LocalDateTime") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30)

    assert(date.before(sample).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusSeconds(100)))
    )
  }

  test("date.after: ZonedDateTime") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).atZone(ZoneOffset.UTC)

    assert(date.after(sample).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusSeconds(100)))
    )
  }

  test("date.before: ZonedDateTime") {
    val sample = LocalDateTime.of(2021, 11, 29, 12, 30).atZone(ZoneOffset.UTC)

    assert(date.before(sample).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusSeconds(100)))
    )
  }

  test("date.after: LocalDate") {
    val sample = LocalDate.of(2021, 11, 29)

    assert(date.after(sample).run(sample.plusDays(1)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusDays(1)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusDays(1)))
    )
  }

  test("date.before: LocalDate") {
    val sample = LocalDate.of(2021, 11, 29)

    assert(date.before(sample).run(sample.minusDays(1)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusDays(1)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusDays(1)))
    )
  }

  test("date.after: LocalTime") {
    val sample = LocalTime.of(12, 30)

    assert(date.after(sample).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusSeconds(100)))
    )
  }

  test("date.before: LocalTime") {
    val sample = LocalTime.of(12, 30)

    assert(date.before(sample).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusSeconds(100)))
    )
  }

  test("date.after: LocalTime") {
    val sample = LocalTime.of(12, 30).atOffset(ZoneOffset.UTC)

    assert(date.after(sample).run(sample.plusSeconds(100)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusSeconds(100)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusSeconds(100)))
    )
  }

  test("date.before: LocalTime") {
    val sample = LocalTime.of(12, 30).atOffset(ZoneOffset.UTC)

    assert(date.before(sample).run(sample.minusSeconds(100)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusSeconds(100)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusSeconds(100)))
    )
  }

  test("date.after: Year") {
    val sample = Year.of(2021)

    assert(date.after(sample).run(sample.plusYears(1)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusYears(1)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusYears(1)))
    )
  }

  test("date.before: Year") {
    val sample = Year.of(2021)

    assert(date.before(sample).run(sample.minusYears(1)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusYears(1)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusYears(1)))
    )
  }

  test("date.after: YearMonth") {
    val sample = YearMonth.of(2021, 11)

    assert(date.after(sample).run(sample.plusMonths(1)).isValid)
    assertEquals(
      obtained = date.after(sample).run(sample.minusMonths(1)).error,
      expected = Some(Validation.Error.Date.After(sample, sample.minusMonths(1)))
    )
  }

  test("date.before: YearMonth") {
    val sample = YearMonth.of(2021, 11)

    assert(date.before(sample).run(sample.minusMonths(1)).isValid)
    assertEquals(
      obtained = date.before(sample).run(sample.plusMonths(1)).error,
      expected = Some(Validation.Error.Date.Before(sample, sample.plusMonths(1)))
    )
  }

  test("number.greaterThan (equal: false)") {
    assert(number.greaterThan(1, equal = false).run(3).isValid)
    assertEquals(
      obtained = number.greaterThan(3, equal = false).run(3).error,
      expected = Some(Validation.Error.Number.GreaterThan(equal = false, 3, 3))
    )
    assertEquals(
      obtained = number.greaterThan(3, equal = false).run(1).error,
      expected = Some(Validation.Error.Number.GreaterThan(equal = false, 3, 1))
    )
  }

  test("number.greaterThan (equal: true)") {
    assert(number.greaterThan(1, equal = true).run(3).isValid)
    assert(number.greaterThan(3, equal = true).run(3).isValid)

    assertEquals(
      obtained = number.greaterThan(3, equal = true).run(1).error,
      expected = Some(Validation.Error.Number.GreaterThan(equal = true, 3, 1))
    )
  }

  test("number.lessThan (equal: false)") {
    assert(number.lessThan(3, equal = false).run(1).isValid)
    assertEquals(
      obtained = number.lessThan(3, equal = false).run(3).error,
      expected = Some(Validation.Error.Number.LessThan(equal = false, 3, 3))
    )

    assertEquals(
      obtained = number.lessThan(1, equal = false).run(3).error,
      expected = Some(Validation.Error.Number.LessThan(equal = false, 1, 3))
    )
  }

  test("number.lessThan (equal: true)") {
    assert(number.lessThan(3, equal = true).run(1).isValid)
    assert(number.lessThan(3, equal = true).run(3).isValid)

    assertEquals(
      obtained = number.lessThan(1, equal = true).run(3).error,
      expected = Some(Validation.Error.Number.LessThan(equal = true, 1, 3))
    )
  }

  test("number.equal") {
    assert(number.equal(3).run(3).isValid)
    assert(number.equal(3f).run(3f).isValid)
    assert(number.equal(3d).run(3d).isValid)

    assertEquals(
      obtained = number.equal(1).run(3).error,
      expected = Some(Validation.Error.Number.Equal(1, 3))
    )
    assertEquals(
      obtained = number.equal(0.3d).run(0.1d + 0.2d).error,
      expected = Some(Validation.Error.Number.Equal(expected = 0.3d, actual = 0.1d + 0.2d))
    )

    assert(number.equal(expected = 0.3d, delta = 0.01d).run(0.1d + 0.2d).isValid)
  }

  test("parsing.bigDecimal") {
    assert(parsing.bigDecimal.run("3.14").isValid)
    assert(parsing.bigDecimal.run("3").isValid)
    assertEquals(
      obtained = parsing.bigDecimal.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.BigDecimal, "foobar"))
    )
  }

  test("parsing.bigInt") {
    assert(parsing.bigInt.run("3").isValid)
    assert(parsing.bigInt.run("0").isValid)
    assertEquals(
      obtained = parsing.bigInt.run("3.14").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.BigInt, "3.14"))
    )
    assertEquals(
      obtained = parsing.bigInt.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.BigInt, "foobar"))
    )
  }

  test("parsing.double") {
    assert(parsing.double.run("3.14").isValid)
    assert(parsing.double.run("3").isValid)
    assertEquals(
      obtained = parsing.double.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Double, "foobar"))
    )
  }

  test("parsing.float") {
    assert(parsing.float.run("3.14").isValid)
    assert(parsing.float.run("3").isValid)
    assertEquals(
      obtained = parsing.float.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Float, "foobar"))
    )
  }

  test("parsing.int") {
    assert(parsing.int.run("3").isValid)
    assert(parsing.int.run("0").isValid)
    assertEquals(
      obtained = parsing.int.run("3.14").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Int, "3.14"))
    )
    assertEquals(
      obtained = parsing.int.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Int, "foobar"))
    )
  }

  test("parsing.long") {
    assert(parsing.long.run("3").isValid)
    assert(parsing.long.run("0").isValid)
    assertEquals(
      obtained = parsing.long.run("3.14").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Long, "3.14"))
    )
    assertEquals(
      obtained = parsing.long.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Long, "foobar"))
    )
  }

  test("parsing.short") {
    assert(parsing.short.run("3").isValid)
    assert(parsing.short.run("0").isValid)
    assertEquals(
      obtained = parsing.short.run("3.14").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Short, "3.14"))
    )
    assertEquals(
      obtained = parsing.short.run("foobar").error,
      expected = Some(Validation.Error.Parsing(Validation.Parsing.Value.Short, "foobar"))
    )
  }

  test("text.atLeast") {
    assert(text.atLeast(reference = 1, equal = true).run("foo").isValid)
    assert(text.atLeast(reference = 3, equal = true).run("foo").isValid)
    assertEquals(
      obtained = text.atLeast(reference = 3, equal = true).run("fo").error,
      expected = Some(Validation.Error.Text.AtLeast(equal = true, reference = 3, actual = 2))
    )
  }

  test("text.atMost") {
    assert(text.atMost(reference = 3, equal = true).run("fo").isValid)
    assert(text.atMost(reference = 3, equal = true).run("foo").isValid)
    assertEquals(
      obtained = text.atMost(reference = 1, equal = true).run("foo").error,
      expected = Some(Validation.Error.Text.AtMost(equal = true, reference = 1, actual = 3))
    )
  }

  test("text.isEmpty") {
    assert(text.empty.run("").isValid)
    assertEquals(
      obtained = text.empty.run("foo").error,
      expected = Some(Validation.Error.Text.AtMost(equal = true, reference = 0, actual = 3))
    )
  }

  test("text.nonEmpty") {
    assert(text.nonEmpty.run("foobar").isValid)
    assertEquals(
      obtained = text.nonEmpty.run("").error,
      expected = Some(Validation.Error.Not(Validation.Error.Text.AtMost(equal = true, reference = 0, actual = 0)))
    )
  }

  test("text.exactly") {
    assert(text.exactly(expected = 3).run("foo").isValid)
    assert(text.exactly(expected = 0).run("").isValid)
    assertEquals(
      obtained = text.exactly(expected = 1).run("foo").error,
      expected = Some(Validation.Error.Text.Exactly(expected = 1, actual = 3))
    )
  }

  test("text.matches") {
    val Whitespace = "\\s+".r

    assert(text.matches(regex = Whitespace).run("   ").isValid)
    assertEquals(
      obtained = text.matches(regex = Whitespace).run(" foobar ").error,
      expected = Some(Validation.Error.Text.Matches(regex = Whitespace, " foobar "))
    )
    assertEquals(
      obtained = text.matches(regex = Whitespace).run("").error,
      expected = Some(Validation.Error.Text.Matches(regex = Whitespace, ""))
    )
  }

  test("text.email") {
    assert(text.email.run("mail@taig.io").isValid)
    assertEquals(
      obtained = text.email.run("foobar").error,
      expected = Some(Validation.Error.Text.Email("foobar"))
    )
  }
}
