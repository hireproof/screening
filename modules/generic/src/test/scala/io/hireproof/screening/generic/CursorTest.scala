package io.hireproof.screening.generic

import cats.data.Validated
import cats.syntax.all._
import io.hireproof.screening.Validation
import io.hireproof.screening.validations._
import munit.FunSuite

final class CursorTest extends FunSuite {
  val requiredError: Validation.Error = Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0)

  test("modifyHistory: success") {
    val obtained = Cursor
      .withHistory(__ / "foo" / "bar", "")
      .modifyHistory(__ / "prefix" ++ _)

    val expected = Cursor.withHistory(__ / "prefix" / "foo" / "bar", "")

    assertEquals(obtained, expected)
  }

  test("modifyHistory: failure") {
    val obtained = Cursor
      .withHistory(__ / "foo" / "bar", "")
      .validate(Validation.invalidNel(Validation.Error.Unknown("x")))
      .modifyHistory(_ / "suffix")

    val expected = Cursor.Failure(Cursor.Errors.oneNel(__ / "foo" / "bar" / "suffix", Validation.Error.Unknown("x")))

    assertEquals(obtained, expected)
  }

  test("andThen: success") {
    case class Bar(value: String)

    case class Foo(bar: Option[Bar])

    val obtained = Cursor.root(Foo(Some(Bar("foobar")))).option("bar", _.bar).flatMap { bar =>
      Cursor.root(bar).field("value", _.value).validate(text.required)
    }

    val expected = Cursor.Success[Option, String](Some(Cursor.Value(__ / "bar" / "value", "foobar")))

    assertEquals(obtained, expected)
  }

  test("andThen: failure") {
    case class Bar(value: String)

    case class Foo(bar: Option[Bar])

    val obtained = Cursor.root(Foo(Some(Bar("")))).option("bar", _.bar).flatMap { bar =>
      Cursor.root(bar).field("value", _.value).validate(text.required)
    }

    val expected = Cursor.Failure(Cursor.Errors.oneNel(__ / "bar" / "value", requiredError))

    assertEquals(obtained, expected)
  }

  test("field: success") {
    case class Foo(bar: String)
    case class Bar(foo: Foo)

    val obtained = Cursor.root(Bar(Foo(""))).field("foo", _.foo).field("bar", _.bar)

    val expected = Cursor.withHistory(__ / "foo" / "bar", "")

    assertEquals(obtained, expected)
  }

  test("field: failure") {
    case class Foo(bar: String)
    case class Bar(foo: Foo)

    val obtained = Cursor
      .root(Bar(Foo("")))
      .field("foo", _.foo)
      .field("bar", _.bar)
      .validate(text.required)

    val expected = Cursor.Failure(Cursor.Errors.oneNel(__ / "foo" / "bar", requiredError))

    assertEquals(obtained, expected)
  }

  test("collection: success") {
    case class Foo(bars: List[String])

    val values = List("foo", "bar", "baz")

    val obtained = Cursor.root(Foo(values)).collection("bars", _.bars)

    val expected = Cursor
      .Success(values.mapWithIndex((value, index) => Cursor.Value(__ / index, value)))
      .modifyHistory(__ / "bars" ++ _)

    assertEquals(obtained, expected)
  }

  test("collection: failure") {
    case class Foo(bars: List[String])

    val obtained = Cursor
      .root(Foo(List("", "bar", "", "baz")))
      .collection("bars", _.bars)
      .validate(text.required)

    val expected = Cursor.Failure(
      Cursor.Errors.ofError(__ / 0 -> requiredError, __ / 2 -> requiredError).modifyHistory(__ / "bars" ++ _)
    )

    assertEquals(obtained, expected)
  }

//  test("field + collection: success") {
//    case class Foo(bars: List[String])
//
//    case class Name(value: String)
//
//    val values = List("foo", "bar", "baz")
//
//    val obtained = Cursor
//      .root(Foo(values))
//      .field("bars", _.bars)
//      .thenValidate(CursorValidation(_.collection.runWith(lift(Name.apply))))
//      .ensure(collection.atMost[List, Name](3))
//
//    val expected = Cursor.Result.Success.id(Cursor.Value(__ / "bars", values.map(Name.apply)))
//
//    assertEquals(obtained, expected)
//  }

//  test("field + collection: failure (inner)") {
//    case class Foo(bars: List[String])
//
//    case class Name(value: String)
//
//    val values = List("foo", "", "baz")
//
//    val obtained = Cursor
//      .root(Foo(values))
//      .field("bars", _.bars)
//      .thenValidate(CursorValidation(_.collection.runWith(text.required.map(Name.apply))))
//      .ensure(collection.atMost[List, Name](3))
//      .get
//
//    val expected = Cursor.Result.Failure(Cursor.Errors.oneNel(__ / "bars" / 1, requiredError))
//
//    assertEquals(obtained, expected)
//  }
//
//  test("field + collection: failure (outer)") {
//    case class Foo(bars: List[String])
//
//    case class Name(value: String)
//
//    val values = List("foo", "bar", "baz")
//
//    val obtained = Cursor
//      .root(Foo(values))
//      .field("bars", _.bars)
//      .thenValidate(CursorValidation(_.collection.runWith(text.required.map(Name.apply))))
//      .ensure(collection.atMost[List, Name](1))
//      .get
//
//    val expected = Cursor.Result.Failure(
//      Cursor.Errors.oneNel(
//        __ / "bars",
//        Validation.Error.Collection.AtMost(equal = true, reference = 1, actual = values.length)
//      )
//    )
//
//    assertEquals(obtained, expected)
//  }
//
//  test("option: success") {
//    case class Foo(bar: String)
//    case class Bar(foo: Option[Foo])
//
//    val obtained = Cursor
//      .root(Bar(Some(Foo(""))))
//      .option("foo", _.foo)
//      .field("bar", _.bar)
//      .get
//
//    val expected = Cursor.Result.Success[Option, String](Some(Cursor.Value(__ / "foo" / "bar", "")))
//
//    assertEquals(obtained, expected)
//  }
//
//  test("option: failure") {
//    case class Foo(bar: String)
//    case class Bar(foo: Option[Foo])
//
//    val obtained = Cursor
//      .root(Bar(Some(Foo(""))))
//      .option("foo", _.foo)
//      .field("bar", _.bar)
//      .validate(text.required)
//      .get
//
//    val expected = Cursor.Result.Failure(Cursor.Errors.oneNel(__ / "foo" / "bar", requiredError))
//
//    assertEquals(obtained, expected)
//  }
//
//  test("oneOf: success") {
//    sealed abstract class Animal extends Product with Serializable
//
//    object Animal {
//      final case class Cat(name: String) extends Animal
//      final case class Dog(goodBoy: Boolean) extends Animal
//    }
//
//    final case class Zoo(animal: Animal)
//
//    val obtained = Cursor
//      .root(Zoo(Animal.Dog(goodBoy = true)))
//      .field("animal", _.animal)
//      .oneOf {
//        case Animal.Cat(_) => "cat" -> Validated.valid("foo")
//        case Animal.Dog(_) => "dog" -> Validated.valid("bar")
//      }
//      .get
//
//    val expected = Cursor.Result.Success.id(Cursor.Value(__ / "animal" / "dog", "bar"))
//
//    assertEquals(obtained, expected)
//  }
//
//  test("oneOf: failure") {
//    sealed abstract class Animal extends Product with Serializable
//
//    object Animal {
//      final case class Cat(name: String) extends Animal
//      final case class Dog(goodBoy: Boolean) extends Animal
//    }
//
//    final case class Zoo(animal: Animal)
//
//    val obtained = Cursor
//      .root(Zoo(Animal.Cat(name = "")))
//      .field("animal", _.animal)
//      .oneOf {
//        case Animal.Cat(name) => "cat" -> text.required.run(name).leftMap(Cursor.Errors.root)
//        case Animal.Dog(_)    => "dog" -> Validated.valid("bar")
//      }
//      .get
//
//    val expected = Cursor.Result.Failure(Cursor.Errors.oneNel(__ / "animal" / "cat", requiredError))
//
//    assertEquals(obtained, expected)
//  }
}
