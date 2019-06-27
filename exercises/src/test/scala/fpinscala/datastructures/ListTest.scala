package fpinscala.datastructures

import fpinscala.datastructures.List.map
import org.scalatest.{FunSpec, Matchers}

class ListTest extends FunSpec with Matchers {

  describe("List length") {
    it ("should return 0 for empty list") {
      List.length(Nil) shouldBe 0
    }

    it ("should return number of elements for non-empty list") {
      List.length(List(11, 12, 13, 14, 15)) shouldBe 5
    }
  }

  describe("List map") {
    it ("should apply function to each element of list") {
      val l = List(1, 2, 3, 5)
      map(l) (_ + 1) shouldBe List(2, 3, 4, 6)
    }
  }

}
