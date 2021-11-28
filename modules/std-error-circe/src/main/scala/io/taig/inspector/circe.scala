package io.taig.inspector

import io.circe.syntax._
import cats.syntax.all._
import io.circe.{Decoder, Encoder, HCursor, JsonObject}

trait circe {
  private object Keys {
    val Context = "context"

    val Type = "type"

    val Expected = "expected"

    val Actual = "actual"

    val Reference = "reference"
  }

  private object Contexts {
    val Collection = "collection"
  }

  private object Types {
    val AtLeast = "atLeast"

    val AtMost = "atMost"

    val Contains = "contains"
  }

  private object encoder {
    def apply[A: Encoder, B: Encoder](context: String, tpe: String, key: String, value: A, actual: B): JsonObject =
      JsonObject(Keys.Context := context, Keys.Type := tpe, key := value, Keys.Actual := actual)

    def expected[A: Encoder, B: Encoder](context: String, tpe: String, expected: A, actual: B): JsonObject =
      encoder(context, tpe, key = Keys.Expected, expected, actual)

    def reference[A: Encoder, B: Encoder](context: String, tpe: String, reference: A, actual: B): JsonObject =
      encoder(context, tpe, key = Keys.Reference, reference, actual)
  }

  private object decoder {
    def apply[A: Decoder, B: Decoder](cursor: HCursor, key: String): (Decoder.Result[A], Decoder.Result[B]) =
      (cursor.get[A](key), cursor.get[B](Keys.Actual))

    def expected[A: Decoder, B: Decoder](cursor: HCursor): (Decoder.Result[A], Decoder.Result[B]) =
      decoder(cursor, Keys.Expected)
  }

  implicit val encoderInspectorError: Encoder.AsObject[Error] = Encoder.AsObject.instance {
    case Error.Collection.AtLeast(expected, actual) =>
      encoder.expected(Contexts.Collection, Types.AtLeast, expected, actual)
    case Error.Collection.AtMost(expected, actual) =>
      encoder.expected(Contexts.Collection, Types.AtMost, expected, actual)
    case Error.Collection.Contains(reference, actual) =>
      encoder.reference(Contexts.Collection, Types.Contains, reference, actual)
  }

  implicit val decoderInspectorError: Decoder[Error] = Decoder.instance { cursor =>
    (cursor.get[String](Keys.Context), cursor.get[String](Keys.Type)).tupled.flatMap {
      case (Contexts.Collection, Types.AtLeast) =>
        decoder.expected[Int, Int](cursor).mapN(Error.Collection.AtLeast)
    }
  }
}

object circe extends circe
