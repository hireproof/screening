package io.taig.inspector

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._
import io.circe.syntax._
import io.circe.{CursorOp, Json}
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

  test("ValidatingDecoder: failure") {
    val decoder = ValidatingDecoder[Int]

    assertEquals(
      obtained = decoder.decodeJson(Json.fromString("3.14")),
      expected = Validated.invalid(ValidatingDecoder.Errors.one(Nil, Left("Int")))
    )
  }

  test("ValidatingDecoder: error") {
    val decoder = ValidatingDecoder[String].ensure(validations.text.required)

    assertEquals(
      obtained = decoder.decodeJson(Json.fromString("")),
      expected = Validated.invalid(
        ValidatingDecoder.Errors.fromErrors(
          Nil,
          NonEmptyList.one(Validation.Error.Not(Validation.Error.Text.AtMost(equal = true, 0, 0)))
        )
      )
    )
  }

  test("ValidatingDecoder: accumulation") {
    val decoder = ValidatingDecoder.instance[(String, Int)] { cursor =>
      val name = cursor.downField("name").validateWith(validations.text.required)
      val age = cursor
        .downField("age")
        .validateWith(
          (validations.number.lessThan(100, equal = true) and validations.number.lessThan(reference = 80)).tap
        )

      (name, age).tupled
    }

    assertEquals(
      obtained = decoder.decodeJson(Json.obj("name" := "", "age" := 130)),
      expected = Validated.invalid(
        ValidatingDecoder.Errors.of(
          List(CursorOp.DownField("age")) ->
            Right(
              NonEmptyList.of(
                Validation.Error.Number.LessThan(equal = true, 100, 130),
                Validation.Error.Number.LessThan(equal = false, 80, 130)
              )
            ),
          List(CursorOp.DownField("name")) ->
            Right(NonEmptyList.one(Validation.Error.Text.AtLeast(equal = false, 0, 0)))
        )
      )
    )
  }
}
