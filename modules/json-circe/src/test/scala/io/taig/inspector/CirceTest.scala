package io.taig.inspector

import cats.data.{NonEmptyList, NonEmptyMap, Validated}
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import io.taig.inspector.circe._
import munit.FunSuite

final class CirceTest extends FunSuite {
  val json: Json = Json.obj(
    "foo" := Json.arr(
      Json.obj(),
      Json.obj(),
      Json.obj("name" := "taig"),
      Json.obj()
    ),
    "bar" := Json.obj(
      "name" := "inspector"
    )
  )

  test("toSelectionHistory") {
    assertEquals(
      obtained =
        toSelectionHistory(json.hcursor.downField("foo").downArray.right.left.right.right.downField("name").history),
      expected = Selection.History.of(Selection.Field("name"), Selection.Index(2), Selection.Field("foo"))
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("bar").downField("name").history),
      expected = Selection.History.of(Selection.Field("name"), Selection.Field("bar"))
    )
  }

  test("toSelectionHistory: deleteGoParent") {
    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").delete.history),
      expected = Selection.History.Root
    )

    assertEquals(
      obtained =
        toSelectionHistory(json.hcursor.downField("foo").downArray.right.right.downField("name").delete.history),
      expected = Selection.History.of(Selection.Index(2), Selection.Field("foo"))
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").downArray.right.right.delete.history),
      expected = Selection.History.of(Selection.Field("foo"))
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.delete.history),
      expected = Selection.History.Root
    )
  }

  test("toSelectionHistory: moveUp") {
    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").up.history),
      expected = Selection.History.Root
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").downArray.right.right.downField("name").up.history),
      expected = Selection.History.of(Selection.Index(2), Selection.Field("foo"))
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").downArray.right.right.up.history),
      expected = Selection.History.of(Selection.Field("foo"))
    )

    assertEquals(
      obtained = toSelectionHistory(json.hcursor.up.history),
      expected = Selection.History.Root
    )
  }

  test("toSelectionHistory: field") {
    assertEquals(
      obtained = toSelectionHistory(json.hcursor.downField("foo").field("bar").history),
      expected = Selection.History.of(Selection.Field("bar"))
    )
  }

  test("ValidatingDecoder: failure") {
    val decoder = ValidatingDecoder[Int]

    assertEquals(
      obtained = decoder.decodeJson(Json.fromString("3.14")),
      expected = Validated.invalid(ValidatingDecoder.Errors(NonEmptyMap.one(Selection.History.Root, Left("Int"))))
    )
  }

  test("ValidatingDecoder: error") {
    val decoder = ValidatingDecoder[String].ensure(rule.text.required)

    assertEquals(
      obtained = decoder.decodeJson(Json.fromString("")),
      expected = Validated.invalid(
        ValidatingDecoder.Errors.one(
          Selection.History.Root,
          NonEmptyList.one(Validation.Error.Not(Validation.Error.Text.AtMost(equal = true, 0, 0)))
        )
      )
    )
  }

  test("ValidatingDecoder: accumulation") {
    val decoder = ValidatingDecoder.instance[(String, Int)] { cursor =>
      val name = cursor.downField("name").validateWith(rule.text.required)
      val age = cursor
        .downField("age")
        .validateWith(
          (rule.number.lessThan(100, equal = true) and rule.number.lessThan(reference = 80)).tap
        )

      (name, age).tupled
    }

    assertEquals(
      obtained = decoder.decodeJson(Json.obj("name" := "", "age" := 130)),
      expected = Validated.invalid(
        ValidatingDecoder.Errors(
          NonEmptyMap.of(
            Selection.History.of(Selection.Field("age")) ->
              Right(
                NonEmptyList.of(
                  Validation.Error.Number.LessThan(equal = true, 100, 130),
                  Validation.Error.Number.LessThan(equal = false, 80, 130)
                )
              ),
            Selection.History.of(Selection.Field("name")) ->
              Right(NonEmptyList.one(Validation.Error.Text.AtLeast(equal = false, 0, 0)))
          )
        )
      )
    )
  }
}
