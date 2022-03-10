package io.hireproof.screening

import munit.FunSuite

final class SelectionHistoryTest extends FunSuite {
  test("toJsonPath") {
    val history = hist / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = history.toJsonPath, expected = ".baz[3].bar.foo")
  }

  test("toJsonPath: root") {
    assertEquals(obtained = hist.toJsonPath, expected = ".")
  }

  test("toJsonPath: array") {
    val history = hist / 3 / "foo"
    assertEquals(obtained = history.toJsonPath, expected = "[3].foo")
  }

  test("toList") {
    val history = hist / "baz" / 3 / "bar"
    assertEquals(
      obtained = history.toList,
      expected = List(Selection.Field("baz"), Selection.Index(3), Selection.Field("bar"))
    )
  }

  test("up") {
    val history = hist / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = history.up, expected = hist / "baz" / 3 / "bar")
  }

  test("/:") {
    val history = hist / "foo" / "bar"
    assertEquals(obtained = "baz" /: history, expected = hist / "baz" / "foo" / "bar")
  }

  test("parse") {
    assertEquals(
      obtained = Selection.History.parse(".foo.bar[3].baz"),
      expected = Right(hist / "foo" / "bar" / 3 / "baz")
    )
  }

  test("parse: root") {
    assertEquals(
      obtained = Selection.History.parse("."),
      expected = Right(hist)
    )
  }

  test("parse: array") {
    assertEquals(
      obtained = Selection.History.parse("[3].foo"),
      expected = Right(hist / 3 / "foo")
    )
  }

  test("parse: empty") {
    assertEquals(
      obtained = Selection.History.parse(""),
      expected = Left("Empty")
    )
  }

  test("parse: invalid characters") {
    assertEquals(
      obtained = Selection.History.parse(".foo I don't belong here .bar[3].baz"),
      expected = Left("Contains invalid characters")
    )
  }

  test("parse: exhaustive") {
    assertEquals(
      obtained = Selection.History.parse(".foo.bar[3].baz I don't belong here"),
      expected = Left("Contains invalid characters")
    )
  }
}
