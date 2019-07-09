package fpinscala.errorhandling

import org.scalatest.{FunSpec, Matchers}

class EitherTest extends FunSpec with Matchers {
  describe("Either map") {
    it ("should not map Left") {
      val either: Either[String, Int] = Left("error")
      either.map(_ * 2) shouldBe Left("error")
    }

    it ("should map Right") {
      Right(5).map(_ * 2) shouldBe Right(10)
    }
  }

  describe("Either flatMap") {
    it ("should not flatMap Left") {
      val either: Either[String, Int] = Left("error")
      either.flatMap(v => Right(v * 2)) shouldBe Left("error")
    }

    it ("should flatMap Right") {
      Right(5).flatMap(v => Right(v * 2)) shouldBe Right(10)
    }
  }

  describe("Either orElse") {
    it ("should return default for Left") {
      val either: Either[String, Int] = Left("error")
      either.orElse(Right(2)) shouldBe Right(2)
    }

    it ("should return value from Right") {
      Right(5).orElse(Right(2)) shouldBe Right(5)
    }
  }

  describe("Either map2") {
    it ("should be Left if both Left") {
      val either: Either[String, Int] = Left("error")
      val either2: Either[Int, Int] = Left(2)
      either.map2(either2)((v: Int, w: Int) => v + w) shouldBe Left("error")
    }

    it ("should be Left if first Left") {
      val either: Either[String, Int] = Left("error")
      either.map2(Right(2))((v: Int, w: Int) => v + w) shouldBe Left("error")
    }

    it ("should be Left if second Left") {
      val either: Either[String, Int] = Right(5)
      either.map2(Left(2))((v: Int, w: Int) => v + w) shouldBe Left(2)
    }

    it ("should be calculated if both Right") {
      val either: Either[String, Int] = Right(5)
      either.map2(Right(2))((v: Int, w: Int) => v + w) shouldBe Right(7)
    }
  }


  describe("sequence") {
    it ("should return Right(Nil) for Nil list") {
      Either.sequence(List()) shouldBe Right(Nil)
    }

    it ("should return Left for list with only Left") {
      Either.sequence(List(Left("a"))) shouldBe Left("a")
    }

    it ("should return first Left for list with at least one Left") {
      Either.sequence(List(Right(2), Left("a"), Left("b"), Right(4))) shouldBe Left("a")
    }

    it ("should return all values when there is no Left element") {
      Either.sequence(List(Right(2), Right(4))) shouldBe Right(List(2, 4))
    }
  }

  describe("traverse") {
    it ("should return Some(Nil) for Nil list") {
      Either.traverse(List())(e => Right(e.toString)) shouldBe Right(Nil)
    }

    it ("should return Left for list with only Left") {
      Either.traverse(List(Left("a")))(e => e flatMap( v => Right(v.toString))) shouldBe Left("a")
    }

    it ("should return first Left for list with at least one Left") {
      Either.traverse(List(Right(2), Left("a"), Left("b"), Right(4)))(e => e flatMap( v => Right(v.toString))) shouldBe Left("a")
    }

    it ("should return all value when there is no Left element") {
      Either.traverse(List(Right(2), Right(4)))(e => e flatMap( v => Right(v.toString))) shouldBe Right(List("2", "4"))
    }
  }

}
