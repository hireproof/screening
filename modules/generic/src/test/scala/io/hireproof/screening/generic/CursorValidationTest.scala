package io.hireproof.screening.generic

import cats.data.Validated
import cats.syntax.all._
import io.hireproof.screening.Validation
import io.hireproof.screening.validations._
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

    val validation: CursorValidation[User, (Age, City)] = CursorValidation { cursor =>
      val age = cursor
        .field("age", _.age)
        .runWith(Age.validation)

      val city = cursor
        .field("address", _.address)
        .field("city", _.city)
        .runWith(City.validation)

      (age, city).tupled
    }

    assertEquals(
      obtained = validation.run(User(age = 21, User.Address("Berlin"))),
      expected = Validated.valid((Age(21), City("Berlin")))
    )

    assertEquals(
      obtained = validation.run(User(age = 12, User.Address(""))),
      expected = Validated.invalid(
        Cursor.Errors.ofError(
          __ / "age" -> Validation.Error.Number.GreaterThan(equal = true, reference = 18, actual = 12),
          __ / "address" / "city" -> Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0)
        )
      )
    )
  }

  test("option") {
    final case class User(name: Option[String])

    final case class Name(value: String)

    object Name {
      val validation: Validation[String, Name] = text.required.map(apply)
    }

    val validation: CursorValidation[User, Option[Name]] = CursorValidation { cursor =>
      cursor.option("name", _.name).runWith(Name.validation)
    }

    assertEquals(obtained = validation.run(User(Some("taig"))), expected = Validated.valid(Some(Name("taig"))))
    assertEquals(obtained = validation.run(User(None)), expected = Validated.valid(None))

    assertEquals(
      obtained = validation.run(User(Some(""))),
      expected = Validated.invalid(
        Cursor.Errors.oneNel(
          __ / "name",
          Validation.Error.Text.AtLeast(equal = false, reference = 0, actual = 0)
        )
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
        .runWith(Name.validation)
    }

    assertEquals(
      obtained = validation.run(Users(Nil)),
      expected = Validated.invalid(
        Cursor.Errors.oneNel(__ / "names", Validation.Error.Collection.AtLeast(equal = true, 1, 0))
      )
    )

    assertEquals(
      obtained = validation.run(Users(List("", "foo", ""))),
      expected = Validated.invalid(
        Cursor.Errors.ofError(
          (__ / "names" / 0) -> Validation.Error.Text.AtLeast(equal = false, 0, 0),
          (__ / "names" / 2) -> Validation.Error.Text.AtLeast(equal = false, 0, 0)
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
          val email = cursor.field("email", _.email).runWith(Reference.validation)
          val age = cursor.field("age", _.age).runWith(number.greaterThan(18, equal = true).tap)
          (email, age).tupled
        }
      }

      case object Guest extends User
    }

    final case class Reference(value: String)

    object Reference {
      val validation: Validation[String, Reference] = text.email.tap.map(apply)
    }

    val validation: CursorValidation[User, Reference] = CursorValidation.oneOf {
      case User.Admin(name)  => "admin" -> Reference.validation.run(s"$name@inspector").leftMap(Cursor.Errors.root)
      case user: User.Member => "member" -> User.Member.validation.run(user).map(_._1)
      case User.Guest        => "guest" -> Validated.valid(Reference("unknown"))
    }

    assertEquals(
      obtained = validation.run(User.Admin("taig")),
      expected = Validated.valid(Reference("taig@inspector"))
    )
  }
}
