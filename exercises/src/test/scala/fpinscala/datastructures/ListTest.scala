package fpinscala.datastructures

import fpinscala.datastructures.List._
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

  describe("List flatMap") {
    it ("should apply function to single element list") {
      val l = List(2)
      flatMap(l) (x => List(x, x) ) shouldBe List(2, 2)
    }

    it ("should apply function to each element of list") {
      val l = List(1, 2, 3)
      flatMap(l) (x => List(x, x) ) shouldBe List(1, 1, 2, 2, 3, 3)
    }
  }

  describe("List filter") {
    it ("should filter list") {
      val l = List(1, 2, 3, 5)
      filter(l) (_ < 3) shouldBe List(1, 2)
    }

    it ("should filter list, implemented with flatMap") {
      val l = List(1, 2, 3, 5)
      filterWithFlatMap(l) (_ < 3) shouldBe List(1, 2)
    }
  }

  describe("List zip") {
    it ("should zip Int lists") {
      val l1 = List(1, 2, 3)
      val l2 = List(4, 5, 6)
      zipInt(l1, l2) shouldBe List(5, 7, 9)
    }

    it ("should zip any lists") {
      val l1 = List(1, 2, 3)
      val l2 = List(4, 5, 6)
      zipWith(l1, l2)(_ + _) shouldBe List(5, 7, 9)
    }
  }

}
