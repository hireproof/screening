package io.taig.inspector

import munit.FunSuite

final class SelectionHistoryTest extends FunSuite {
  test("toJsonPath") {
    val history = "foo" :: "bar" :: 3 :: "baz" :: Selection.History.Root
    assertEquals(obtained = history.toJsonPath, expected = ".baz[3].bar.foo")
  }

  test("toJsonPath: root") {
    assertEquals(obtained = Selection.History.Root.toJsonPath, expected = ".")
  }

  test("toJsonPath: array") {
    val history = "foo" :: 3 :: Selection.History.Root
    assertEquals(obtained = history.toJsonPath, expected = "[3].foo")
  }

  test("parse") {
    assertEquals(
      obtained = Selection.History.parse(".foo.bar[3].baz"),
      expected = Right("baz" :: 3 :: "bar" :: "foo" :: Selection.History.Root)
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
      expected = Right("foo" :: 3 :: Selection.History.Root)
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
