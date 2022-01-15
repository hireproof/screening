package io.hireproof.screening

import munit.FunSuite

final class SelectionHistoryTest extends FunSuite {
  test("toJsonPath") {
    val history = __ / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = history.toJsonPath, expected = ".baz[3].bar.foo")
  }

  test("toJsonPath: root") {
    assertEquals(obtained = __.toJsonPath, expected = ".")
  }

  test("toJsonPath: array") {
    val history = __ / 3 / "foo"
    assertEquals(obtained = history.toJsonPath, expected = "[3].foo")
  }

  test("toList") {
    val history = __ / "baz" / 3 / "bar"
    assertEquals(
      obtained = history.toList,
      expected = List(Selection.Field("baz"), Selection.Index(3), Selection.Field("bar"))
    )
  }

  test("up") {
    val history = __ / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = history.up, expected = __ / "baz" / 3 / "bar")
  }

  test("/:") {
    val history = __ / "foo" / "bar"
    assertEquals(obtained = "baz" /: history, expected = __ / "baz" / "foo" / "bar")
  }

  test("parse") {
    assertEquals(
      obtained = Selection.History.parse(".foo.bar[3].baz"),
      expected = Right(__ / "foo" / "bar" / 3 / "baz")
    )
  }

  test("parse: root") {
    assertEquals(
      obtained = Selection.History.parse("."),
      expected = Right(__)
    )
  }

  test("parse: array") {
    assertEquals(
      obtained = Selection.History.parse("[3].foo"),
      expected = Right(__ / 3 / "foo")
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
