package io.taig.inspector

import munit.FunSuite

final class SelectionHistoryTest extends FunSuite {
  test("toJsonPath") {
    val history = Selection.History.fields("foo", "bar") :+ Selection.Index(3) :+ Selection.Field("baz")
    assertEquals(obtained = history.toJsonPath, expected = ".foo.bar[3].baz")
  }

  test("toJsonPath: root") {
    assertEquals(obtained = Selection.History.Root.toJsonPath, expected = ".")
  }

  test("toJsonPath: array") {
    val history = Selection.History.of(Selection.Index(3), Selection.Field("foo"))
    assertEquals(obtained = history.toJsonPath, expected = "[3].foo")
  }

  test("parse") {
    assertEquals(
      obtained = Selection.History.parse(".foo.bar[3].baz"),
      expected = Right(Selection.History.Root / "foo" / "bar" / 3 / "baz")
    )
  }

  test("parse: root") {
    assertEquals(
      obtained = Selection.History.parse("."),
      expected = Right(Selection.History.Root)
    )
  }

  test("parse: array") {
    assertEquals(
      obtained = Selection.History.parse("[3].foo"),
      expected = Right(Selection.History.of(Selection.Index(3), Selection.Field("foo")))
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
