package fpinscala.errorhandling

import org.scalatest.{FunSpec, Matchers}

class OptionTest extends FunSpec with Matchers {
  describe("Option map") {
    it ("should not map None") {
      None.map(_ => 4) shouldBe None
    }

    it ("should map Some") {
      Some(5).map(_ * 2) shouldBe Some(10)
    }
  }

  describe("Option getOrElse") {
    it ("should return default for None") {
      None.getOrElse(4) shouldBe 4
    }

    it ("should return value from Some") {
      Some(5).getOrElse(4) shouldBe 5
    }
  }

  describe("Option flatMap") {
    it ("should not flatMap None") {
      None.flatMap(_ => Some(4)) shouldBe None
    }

    it ("should flatMap Some") {
      Some(5).flatMap(_ => Some(4)) shouldBe Some(4)
    }
  }

  describe("Option orElse") {
    it ("should return default for None") {
      None.orElse(Some(4)) shouldBe Some(4)
    }

    it ("should return value from Some") {
      Some(5).orElse(Some(4)) shouldBe Some(5)
    }
  }

  describe("Option filter") {
    it ("should not change None") {
      None.filter(_ => false) shouldBe None
    }

    it ("should filter Some if it doesn't match") {
      Some(5).filter(_ > 100) shouldBe None
    }

    it ("should not changes Some if it matches") {
      Some(5).filter(_ > 4) shouldBe Some(5)
    }
  }

  describe("Seq variance") {
    it ("should be None for empty list") {
      Option.variance( Seq() ) shouldBe None
    }

    it ("should be calculated for list with elements") {
      Option.variance( Seq(3, 3, 9) ) shouldBe Some(8)
    }

    it ("should be zero for single element list") {
      Option.variance( Seq(4) ) shouldBe Some(0)
    }
  }

  describe("Option map2") {
    it ("should be None if both None") {
      Option.map2(None, None)( (a, _) => a) shouldBe None
    }

    it ("should be None if first None") {
      Option.map2(None, Some(1)) ( (a, _) => a) shouldBe None
    }

    it ("should be None if second None") {
      Option.map2(Some(1), None) ( (a, _) => a) shouldBe None
    }

    it ("should be calculated if both Some") {
      Option.map2(Some(1), Some(4)) ( (a, b) => a + b) shouldBe Some(5)
    }
  }

  describe("sequence") {
    it ("should return Some(Nil) for Nil list") {
      Option.sequence(List()) shouldBe Some(Nil)
    }

    it ("should return None for list with only None") {
      Option.sequence(List(None)) shouldBe None
    }

    it ("should return None for list with at least one None") {
      Option.sequence(List(Some(2), None, Some(4))) shouldBe None
    }

    it ("should return all value when there is no None element") {
      Option.sequence(List(Some(2), Some(4))) shouldBe Some(List(2, 4))
    }
  }

  describe("traverse") {
    it ("should return Some(Nil) for Nil list") {
      Option.traverse(List())(e => Some(e.toString)) shouldBe Some(Nil)
    }

    it ("should return None for list with only None") {
      Option.traverse(List(None))(e => e flatMap( v => Some(v.toString))) shouldBe None
    }

    it ("should return None for list with at least one None") {
      Option.traverse(List(Some(2), None, Some(4)))(e => e flatMap( v => Some(v.toString))) shouldBe None
    }

    it ("should return all value when there is no None element") {
      Option.traverse(List(Some(2), Some(4)))(e => e flatMap( v => Some(v.toString))) shouldBe Some(List("2", "4"))
    }
  }

}
