package fpinscala.laziness

import org.scalatest.{FunSpec, Matchers}

class StreamTest extends FunSpec with Matchers {

  describe("Stream toList") {
    it ("should convert empty stream to empty list") {
      Stream.empty.toList() shouldBe List()
    }

    it ("should convert stream to list with all elements") {
      Stream(1, 2, 4).toList() shouldBe List(1, 2, 4)
    }
  }

  describe("Stream take") {
    it ("should return empty for empty stream") {
      Stream.empty.take(3) shouldBe Empty
    }

    it ("should take the first n elements") {
      Stream(1, 2, 4).take(2).toList() shouldBe Stream(1, 2).toList()
    }

    it ("should take the first n elements if stream smaller than n") {
      Stream(1, 2, 4).take(24).toList() shouldBe Stream(1, 2, 4).toList()
    }
  }

  describe("Stream drop") {
    it ("should return empty for empty stream") {
      Stream.empty.drop(3) shouldBe Empty
    }

    it ("should drop the first n elements") {
      Stream(1, 2, 4).drop(2).toList() shouldBe Stream(4).toList()
    }

    it ("should drop all elements if stream smaller than n") {
      Stream(1, 2, 4).drop(24) shouldBe Empty
    }
  }

  describe("Stream takeWhile") {
    it ("should return empty for empty stream") {
      Stream.empty[Int].takeWhile(_ > 4) shouldBe Empty
    }

    it ("should return empty stream if predicate fails for all elements") {
      Stream(1, 2, 4).takeWhile(_ > 4) shouldBe Empty
    }

    it ("should return all elements that match predicate until first one that fails") {
      Stream(5, 6, 2, 77).takeWhile(_ > 4).toList() shouldBe Stream(5, 6).toList()
    }
  }

}
