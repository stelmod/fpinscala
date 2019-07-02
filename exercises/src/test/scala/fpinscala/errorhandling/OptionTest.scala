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
}
