package io.taig.inspector

import cats.data.NonEmptyList
import cats.syntax.all._
import io.taig.inspector.validations._
import munit.FunSuite

final class CursorValidationTest extends FunSuite {
  test("field") {
    final case class User(age: Int, address: User.Address)

    object User {
      final case class Address(city: String)
    }

    final case class Age(value: Int)

    object Age {
      val validation: Validation[Int, Age] = number.greaterThan(reference = 18, equal = true).tap.map(apply)
    }

    final case class City(value: String)

    object City {
      val validation: Validation[String, City] = text.required.tap.map(apply)
    }

    val validation = CursorValidation[User, (Age, City)] { cursor =>
      val age = cursor
        .field("age", _.age)
        .run(Age.validation)

      val city = cursor
        .field("address", _.address)
        .field("city", _.city)
        .run(City.validation)

      (age, city).tupled
    }

    assertEquals(
      obtained = validation.run(User(age = 21, User.Address("Berlin"))),
      expected = Cursor.Success.root((Age(21), City("Berlin")))
    )

    assertEquals(
      obtained = validation.run(User(age = 12, User.Address(""))),
      expected = Cursor.Failure.of(
        ("age" :: Selection.History.Root) ->
          NonEmptyList.one(Validation.Error.Number.GreaterThan(equal = true, reference = 18, actual = 12)),
        ("city" :: "address" :: Selection.History.Root) ->
          NonEmptyList.one(Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0))
      )
    )

    assertEquals(
      obtained = ("city" :: "address" :: Selection.History.Root).toJsonPath,
      expected = ".address.city"
    )
  }

  test("option") {
    final case class User(name: Option[String])

    final case class Name(value: String)

    object Name {
      val validation: Validation[String, Name] = text.required.map(apply)
    }

    val validation: CursorValidation[User, Option[Name]] = CursorValidation { cursor =>
      cursor.option("name", _.name).run(Name.validation)
    }

    assertEquals(obtained = validation.run(User(Some("taig"))), expected = Cursor.Success.root(Some(Name("taig"))))
    assertEquals(obtained = validation.run(User(None)), expected = Cursor.Success.root(None))

    assertEquals(
      obtained = validation.run(User(Some(""))),
      expected = Cursor.Failure.one(
        "name" :: Selection.History.Root,
        NonEmptyList.one(Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0))
      )
    )
  }

  test("collection") {
    final case class Users(names: List[String])

    final case class Name(value: String)

    object Name {
      val validation: Validation[String, Name] = text.required.map(apply)
    }

    val validation: CursorValidation[Users, List[Name]] = CursorValidation { cursor =>
      cursor
        .field("names", _.names)
        .ensure(collection.atLeast[List, String](1))
        .collection
        .run(Name.validation)
    }

    assertEquals(
      obtained = validation.run(Users(Nil)),
      expected = Cursor.Failure.one(
        "names" :: Selection.History.Root,
        NonEmptyList.one(Validation.Error.Collection.AtLeast(equal = true, 1, 0))
      )
    )

    assertEquals(
      obtained = validation.run(Users(List("", "foo", ""))),
      expected = Cursor.Failure.of(
        (0 :: "names" :: Selection.History.Root) -> NonEmptyList.one(
          Validation.Error.Text.AtLeast(equal = false, 0, 0)
        ),
        (2 :: "names" :: Selection.History.Root) -> NonEmptyList.one(
          Validation.Error.Text.AtLeast(equal = false, 0, 0)
        )
      )
    )
  }

  test("oneOf") {
    sealed abstract class User extends Product with Serializable

    object User {
      final case class Admin(name: String) extends User
      final case class Member(email: String, age: Int) extends User

      object Member {
        val validation: CursorValidation[User.Member, (Reference, Int)] = CursorValidation { cursor =>
          val email = cursor.field("email", _.email).run(Reference.validation)
          val age = cursor.field("age", _.age).run(number.greaterThan(18, equal = true).tap)
          (email, age).tupled
        }
      }

      final case object Guest extends User
    }

    final case class Reference(value: String)

    object Reference {
      val validation: Validation[String, Reference] = text.email.tap.map(apply)
    }

    val validation: CursorValidation[User, Reference] = CursorValidation { cursor =>
      cursor.oneOfRun {
        case User.Admin(name) => "admin" -> Cursor.Result.fromValidatedNel(Reference.validation.run(s"$name@inspector"))
        case user: User.Member => "member" -> User.Member.validation.run(user).map(_._1)
        case User.Guest        => "guest" -> Cursor.Success.root(Reference("unknown"))
      }
    }

    assertEquals(
      obtained = validation.run(User.Admin("taig")),
      expected = Cursor.Success.root(Reference("taig@inspector"))
    )
  }
}
