package io.taig.inspector

import munit.FunSuite

import java.time._

abstract class ValidationsTest[E] extends FunSuite {
  def validation: Validations[E]

  test("collection.atLeast") {
    assertEquals(
      obtained = validation.collection.atLeast(3).attributes,
      expected = List(Validation.Attribute.Collection.AtLeast(expected = 3))
    )

    assert(validation.collection.atLeast(expected = 1).run(List(1, 2, 3)).isValid)
    assert(validation.collection.atLeast(expected = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = validation.collection.atLeast(expected = 3).run(List(1)).error,
      expected = Some(validation.collection.errors.atLeast(expected = 3, actual = 1))
    )
  }

  test("collection.atMost") {
    assertEquals(
      obtained = validation.collection.atMost(3).attributes,
      expected = List(Validation.Attribute.Collection.AtMost(expected = 3))
    )

    assert(validation.collection.atMost(expected = 3).run(List(1)).isValid)
    assert(validation.collection.atMost(expected = 3).run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = validation.collection.atMost(expected = 1).run(List(1, 2, 3)).error,
      expected = Some(validation.collection.errors.atMost(expected = 1, actual = 3))
    )
  }

  test("collection.isEmpty") {
    assertEquals(
      obtained = validation.collection.empty.attributes,
      expected = List(Validation.Attribute.Collection.AtMost(expected = 0))
    )

    assert(validation.collection.empty.run(Nil).isValid)
    assertEquals(
      obtained = validation.collection.empty.run(List(1, 2, 3)).error,
      expected = Some(validation.collection.errors.atMost(expected = 0, actual = 3))
    )
  }

  test("collection.nonEmpty") {
    assertEquals(
      obtained = validation.collection.nonEmpty.attributes,
      expected = List(Validation.Attribute.Collection.AtLeast(expected = 1))
    )

    assert(validation.collection.nonEmpty.run(List(1, 2, 3)).isValid)
    assertEquals(
      obtained = validation.collection.nonEmpty.run(Nil).error,
      expected = Some(validation.collection.errors.atLeast(expected = 1, actual = 0))
    )
  }

  test("collection.exactly") {
    assertEquals(
      obtained = validation.collection.exactly(3).attributes,
      expected = List(
        Validation.Attribute.Collection.AtLeast(expected = 3),
        Validation.Attribute.Collection.AtMost(expected = 3)
      )
    )

    assert(validation.collection.exactly(expected = 3).run(List(1, 2, 3)).isValid)
    assert(validation.collection.exactly(expected = 0).run(Nil).isValid)
    assertEquals(
      obtained = validation.collection.exactly(expected = 3).run(List(1)).error,
      expected = Some(validation.collection.errors.exactly(expected = 3, actual = 1))
    )
  }

  test("collection.contains") {
    assertEquals(
      obtained = validation.collection.contains(reference = "foobar").attributes,
      expected = List(Validation.Attribute.Collection.Contains(reference = "foobar"))
    )

    assert(validation.collection.contains(reference = "foobar").run(List("foo", "foobar", "bar")).isValid)
    assertEquals(
      obtained = validation.collection.contains(reference = "foobar").run(List("foo", "bar")).error,
      expected = Some(validation.collection.errors.contains(reference = "foobar", actual = List("foo", "bar")))
    )
  }

  test("date.after: OffsetDateTime") {
    val date = OffsetDateTime.of(LocalDateTime.of(2021, 11, 29, 12, 30), ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusDays(1)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusDays(1)).error,
      expected = Some(validation.errors.date.after(date, date.minusDays(1)))
    )
  }

  test("date.before: OffsetDateTime") {
    val date = OffsetDateTime.of(LocalDateTime.of(2021, 11, 29, 12, 30), ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusDays(1)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusDays(1)).error,
      expected = Some(validation.errors.date.before(date, date.plusDays(1)))
    )
  }

  test("date.after: Instant") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusSeconds(100)).error,
      expected = Some(validation.errors.date.after(date, date.minusSeconds(100)))
    )
  }

  test("date.before: Instant") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30).toInstant(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusSeconds(100)).error,
      expected = Some(validation.errors.date.before(date, date.plusSeconds(100)))
    )
  }

  test("date.after: LocalDateTime") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusSeconds(100)).error,
      expected = Some(validation.errors.date.after(date, date.minusSeconds(100)))
    )
  }

  test("date.before: LocalDateTime") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusSeconds(100)).error,
      expected = Some(validation.errors.date.before(date, date.plusSeconds(100)))
    )
  }

  test("date.after: ZonedDateTime") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30).atZone(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusSeconds(100)).error,
      expected = Some(validation.errors.date.after(date, date.minusSeconds(100)))
    )
  }

  test("date.before: ZonedDateTime") {
    val date = LocalDateTime.of(2021, 11, 29, 12, 30).atZone(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusSeconds(100)).error,
      expected = Some(validation.errors.date.before(date, date.plusSeconds(100)))
    )
  }

  test("date.after: LocalDate") {
    val date = LocalDate.of(2021, 11, 29)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusDays(1)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusDays(1)).error,
      expected = Some(validation.errors.date.after(date, date.minusDays(1)))
    )
  }

  test("date.before: LocalDate") {
    val date = LocalDate.of(2021, 11, 29)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusDays(1)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusDays(1)).error,
      expected = Some(validation.errors.date.before(date, date.plusDays(1)))
    )
  }

  test("date.after: LocalTime") {
    val date = LocalTime.of(12, 30)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusSeconds(100)).error,
      expected = Some(validation.errors.date.after(date, date.minusSeconds(100)))
    )
  }

  test("date.before: LocalTime") {
    val date = LocalTime.of(12, 30)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusSeconds(100)).error,
      expected = Some(validation.errors.date.before(date, date.plusSeconds(100)))
    )
  }

  test("date.after: LocalTime") {
    val date = LocalTime.of(12, 30).atOffset(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusSeconds(100)).error,
      expected = Some(validation.errors.date.after(date, date.minusSeconds(100)))
    )
  }

  test("date.before: LocalTime") {
    val date = LocalTime.of(12, 30).atOffset(ZoneOffset.UTC)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusSeconds(100)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusSeconds(100)).error,
      expected = Some(validation.errors.date.before(date, date.plusSeconds(100)))
    )
  }

  test("date.after: Year") {
    val date = Year.of(2021)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusYears(1)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusYears(1)).error,
      expected = Some(validation.errors.date.after(date, date.minusYears(1)))
    )
  }

  test("date.before: Year") {
    val date = Year.of(2021)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusYears(1)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusYears(1)).error,
      expected = Some(validation.errors.date.before(date, date.plusYears(1)))
    )
  }

  test("date.after: YearMonth") {
    val date = YearMonth.of(2021, 11)

    assertEquals(
      obtained = validation.date.after(reference = date).attributes,
      expected = List(Validation.Attribute.Date.After(reference = date))
    )

    assert(validation.date.after(date).run(date.plusMonths(1)).isValid)
    assertEquals(
      obtained = validation.date.after(date).run(date.minusMonths(1)).error,
      expected = Some(validation.errors.date.after(date, date.minusMonths(1)))
    )
  }

  test("date.before: YearMonth") {
    val date = YearMonth.of(2021, 11)

    assertEquals(
      obtained = validation.date.before(reference = date).attributes,
      expected = List(Validation.Attribute.Date.Before(reference = date))
    )

    assert(validation.date.before(date).run(date.minusMonths(1)).isValid)
    assertEquals(
      obtained = validation.date.before(date).run(date.plusMonths(1)).error,
      expected = Some(validation.errors.date.before(date, date.plusMonths(1)))
    )
  }

  test("enumeration.oneOf") {
    assertEquals(
      obtained = validation.enumeration.oneOf(List("foo", "bar")).attributes,
      expected = List(Validation.Attribute.Enumeration.OneOf(List("foo", "bar")))
    )

    assert(validation.enumeration.oneOf(List("foo", "bar")).run("foo").isValid)
    assert(validation.enumeration.oneOf(List("foo", "bar")).run("bar").isValid)
    assertEquals(
      obtained = validation.enumeration.oneOf(List("foo", "bar")).run("foobar").error,
      expected = Some(validation.errors.enumeration.oneOf(List("foo", "bar"), "foobar"))
    )
  }

  test("numeric.greaterThan") {
    assertEquals(
      obtained = validation.numeric.greaterThan(3).attributes,
      expected = List(Validation.Attribute.Numeric.GreaterThan(equals = false, expected = 3))
    )

    assert(validation.numeric.greaterThan(1).run(3).isValid)
    assertEquals(
      obtained = validation.numeric.greaterThan(3).run(3).error,
      expected = Some(validation.numeric.errors.greaterThan(equals = false, 3, 3))
    )
    assertEquals(
      obtained = validation.numeric.greaterThan(3).run(1).error,
      expected = Some(validation.numeric.errors.greaterThan(equals = false, 3, 1))
    )
  }

  test("numeric.greaterThanEquals") {
    assertEquals(
      obtained = validation.numeric.greaterThanEquals(3).attributes,
      expected = List(Validation.Attribute.Numeric.GreaterThan(equals = true, expected = 3))
    )

    assert(validation.numeric.greaterThanEquals(1).run(3).isValid)
    assert(validation.numeric.greaterThanEquals(3).run(3).isValid)

    assertEquals(
      obtained = validation.numeric.greaterThanEquals(3).run(1).error,
      expected = Some(validation.numeric.errors.greaterThan(equals = true, 3, 1))
    )
  }

  test("numeric.lessThan") {
    assertEquals(
      obtained = validation.numeric.lessThan(3).attributes,
      expected = List(Validation.Attribute.Numeric.LesserThan(equals = false, expected = 3))
    )

    assert(validation.numeric.lessThan(3).run(1).isValid)
    assertEquals(
      obtained = validation.numeric.lessThan(3).run(3).error,
      expected = Some(validation.numeric.errors.lessThan(equals = false, 3, 3))
    )

    assertEquals(
      obtained = validation.numeric.lessThan(1).run(3).error,
      expected = Some(validation.numeric.errors.lessThan(equals = false, 1, 3))
    )
  }

  test("numeric.lessThanEquals") {
    assertEquals(
      obtained = validation.numeric.lessThanEquals(3).attributes,
      expected = List(Validation.Attribute.Numeric.LesserThan(equals = true, expected = 3))
    )

    assert(validation.numeric.lessThanEquals(3).run(1).isValid)
    assert(validation.numeric.lessThanEquals(3).run(3).isValid)

    assertEquals(
      obtained = validation.numeric.lessThanEquals(1).run(3).error,
      expected = Some(validation.numeric.errors.lessThan(equals = true, 1, 3))
    )
  }

  test("numeric.equals") {
    assertEquals(
      obtained = validation.numeric.equals(3).attributes,
      expected = List(
        Validation.Attribute.Numeric.GreaterThan(equals = true, expected = 3),
        Validation.Attribute.Numeric.LesserThan(equals = true, expected = 3)
      )
    )

    assert(validation.numeric.equals(3).run(3).isValid)
    assert(validation.numeric.equals(3f).run(3f).isValid)
    assert(validation.numeric.equals(3d).run(3d).isValid)

    assertEquals(
      obtained = validation.numeric.equals(1).run(3).error,
      expected = Some(validation.numeric.errors.equal(1, 3))
    )
    assertEquals(
      obtained = validation.numeric.equals(0.3d).run(0.1d + 0.2d).error,
      expected = Some(validation.numeric.errors.equal(expected = 0.3d, actual = 0.1d + 0.2d))
    )

    assert(validation.numeric.equals(expected = 0.3d, delta = 0.01d).run(0.1d + 0.2d).isValid)
  }

  test("parsing.bigDecimal") {
    assertEquals(
      obtained = validation.parsing.bigDecimal.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.BigDecimal))
    )

    assert(validation.parsing.bigDecimal.run("3.14").isValid)
    assert(validation.parsing.bigDecimal.run("3").isValid)
    assertEquals(
      obtained = validation.parsing.bigDecimal.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.BigDecimal, "foobar"))
    )
  }

  test("parsing.bigInt") {
    assertEquals(
      obtained = validation.parsing.bigInt.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.BigInt))
    )

    assert(validation.parsing.bigInt.run("3").isValid)
    assert(validation.parsing.bigInt.run("0").isValid)
    assertEquals(
      obtained = validation.parsing.bigInt.run("3.14").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.BigInt, "3.14"))
    )
    assertEquals(
      obtained = validation.parsing.bigInt.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.BigInt, "foobar"))
    )
  }

  test("parsing.double") {
    assertEquals(
      obtained = validation.parsing.double.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.Double))
    )

    assert(validation.parsing.double.run("3.14").isValid)
    assert(validation.parsing.double.run("3").isValid)
    assertEquals(
      obtained = validation.parsing.double.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Double, "foobar"))
    )
  }

  test("parsing.float") {
    assertEquals(
      obtained = validation.parsing.float.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.Float))
    )

    assert(validation.parsing.float.run("3.14").isValid)
    assert(validation.parsing.float.run("3").isValid)
    assertEquals(
      obtained = validation.parsing.float.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Float, "foobar"))
    )
  }

  test("parsing.int") {
    assertEquals(
      obtained = validation.parsing.int.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.Int))
    )

    assert(validation.parsing.int.run("3").isValid)
    assert(validation.parsing.int.run("0").isValid)
    assertEquals(
      obtained = validation.parsing.int.run("3.14").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Int, "3.14"))
    )
    assertEquals(
      obtained = validation.parsing.int.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Int, "foobar"))
    )
  }

  test("parsing.long") {
    assertEquals(
      obtained = validation.parsing.long.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.Long))
    )

    assert(validation.parsing.long.run("3").isValid)
    assert(validation.parsing.long.run("0").isValid)
    assertEquals(
      obtained = validation.parsing.long.run("3.14").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Long, "3.14"))
    )
    assertEquals(
      obtained = validation.parsing.long.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Long, "foobar"))
    )
  }

  test("parsing.short") {
    assertEquals(
      obtained = validation.parsing.short.attributes,
      expected = List(Validation.Attribute.Parsing(Validation.Attribute.Parsing.Value.Short))
    )

    assert(validation.parsing.short.run("3").isValid)
    assert(validation.parsing.short.run("0").isValid)
    assertEquals(
      obtained = validation.parsing.short.run("3.14").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Short, "3.14"))
    )
    assertEquals(
      obtained = validation.parsing.short.run("foobar").error,
      expected = Some(validation.parsing.errors.apply(Validation.Attribute.Parsing.Value.Short, "foobar"))
    )
  }

  test("text.atLeast") {
    assertEquals(
      obtained = validation.text.atLeast(reference = 3).attributes,
      expected = List(Validation.Attribute.Text.AtLeast(reference = 3))
    )

    assert(validation.text.atLeast(reference = 1).run("foo").isValid)
    assert(validation.text.atLeast(reference = 3).run("foo").isValid)
    assertEquals(
      obtained = validation.text.atLeast(reference = 3).run("fo").error,
      expected = Some(validation.text.errors.atLeast(reference = 3, actual = 2))
    )
  }

  test("text.atMost") {
    assertEquals(
      obtained = validation.text.atMost(reference = 3).attributes,
      expected = List(Validation.Attribute.Text.AtMost(reference = 3))
    )

    assert(validation.text.atMost(reference = 3).run("fo").isValid)
    assert(validation.text.atMost(reference = 3).run("foo").isValid)
    assertEquals(
      obtained = validation.text.atMost(reference = 1).run("foo").error,
      expected = Some(validation.text.errors.atMost(reference = 1, actual = 3))
    )
  }

  test("text.isEmpty") {
    assertEquals(
      obtained = validation.text.isEmpty.attributes,
      expected = List(Validation.Attribute.Text.AtMost(reference = 0))
    )

    assert(validation.text.isEmpty.run("").isValid)
    assertEquals(
      obtained = validation.text.isEmpty.run("foo").error,
      expected = Some(validation.text.errors.atMost(reference = 0, actual = 3))
    )
  }

  test("text.nonEmpty") {
    assertEquals(
      obtained = validation.text.nonEmpty.attributes,
      expected = List(Validation.Attribute.Text.AtLeast(reference = 1))
    )

    assert(validation.text.nonEmpty.run("foobar").isValid)
    assertEquals(
      obtained = validation.text.nonEmpty.run("").error,
      expected = Some(validation.text.errors.atLeast(reference = 1, actual = 0))
    )
  }

  test("text.exactly") {
    assertEquals(
      obtained = validation.text.exactly(3).attributes,
      expected = List(Validation.Attribute.Text.AtLeast(reference = 3), Validation.Attribute.Text.AtMost(reference = 3))
    )

    assert(validation.text.exactly(reference = 3).run("foo").isValid)
    assert(validation.text.exactly(reference = 0).run("").isValid)
    assertEquals(
      obtained = validation.text.exactly(reference = 1).run("foo").error,
      expected = Some(validation.text.errors.exactly(expected = 1, actual = 3))
    )
  }

  test("text.matches") {
    val Whitespace = "\\s+".r

    assertEquals(
      obtained = validation.text.matches(regex = Whitespace).attributes,
      expected = List(Validation.Attribute.Text.Matches(regex = Whitespace))
    )

    assert(validation.text.matches(regex = Whitespace).run("   ").isValid)
    assertEquals(
      obtained = validation.text.matches(regex = Whitespace).run(" foobar ").error,
      expected = Some(validation.text.errors.matches(regex = Whitespace, " foobar "))
    )
    assertEquals(
      obtained = validation.text.matches(regex = Whitespace).run("").error,
      expected = Some(validation.text.errors.matches(regex = Whitespace, ""))
    )
  }

  test("text.email") {
    assertEquals(
      obtained = validation.text.email.attributes,
      expected = List(Validation.Attribute.Text.Matches(regex = validation.text.EmailRegex))
    )

    assert(validation.text.email.run("mail@taig.io").isValid)
    assertEquals(
      obtained = validation.text.email.run("foobar").error,
      expected = Some(validation.text.errors.email("foobar"))
    )
  }
}
