package fpinscala.datastructures

import org.scalatest.{FunSpec, Matchers}

class TreeTest extends FunSpec with Matchers  {
  describe("Tree size") {
    it ("should return 1 for leaf") {
      Tree.size(Leaf(1)) shouldBe 1
    }

    it ("should return size for shallow tree") {
      Tree.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    }

    it ("should return size for deep(er) tree") {
      Tree.size(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) shouldBe 5
    }
  }

  describe("Tree max") {
    it ("should return value from leaf") {
      Tree.maximum(Leaf(33)) shouldBe 33
    }

    it ("should return max for shallow tree") {
      Tree.maximum(Branch(Leaf(11), Leaf(21))) shouldBe 21
    }

    it ("should return max for deep(er) tree") {
      Tree.maximum(Branch(Leaf(1), Branch(Leaf(4), Leaf(25)))) shouldBe 25
    }
  }

  describe("Tree depth") {
    it ("should return 1 for leaf") {
      Tree.depth(Leaf(1)) shouldBe 0
    }

    it ("should return depth for shallow tree") {
      Tree.depth(Branch(Leaf(1), Leaf(2))) shouldBe 1
    }

    it ("should return depth for deep(er) tree") {
      Tree.depth(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) shouldBe 2
    }
  }

  describe("Tree map") {
    it ("should map leaf") {
      Tree.map(Leaf(1)) (_ * 2) shouldBe Leaf(2)
    }

    it ("should map shallow tree") {
      Tree.map(Branch(Leaf(1), Leaf(2))) (_ * 2) shouldBe Branch(Leaf(2), Leaf(4))
    }

    it ("should map deep(er) tree") {
      Tree.map(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) (_ * 2) shouldBe Branch(Leaf(2), Branch(Leaf(8), Leaf(4)))
    }
  }

  describe("operations implemented with fold") {
    describe("Tree size") {
      it ("should return 1 for leaf") {
        Tree.sizeByFold(Leaf(1)) shouldBe 1
      }

      it ("should return size for shallow tree") {
        Tree.sizeByFold(Branch(Leaf(1), Leaf(2))) shouldBe 3
      }

      it ("should return size for deep(er) tree") {
        Tree.sizeByFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) shouldBe 5
      }
    }

    describe("Tree max") {
      it ("should return value from leaf") {
        Tree.maximumByFold(Leaf(33)) shouldBe 33
      }

      it ("should return max for shallow tree") {
        Tree.maximumByFold(Branch(Leaf(11), Leaf(21))) shouldBe 21
      }

      it ("should return max for deep(er) tree") {
        Tree.maximumByFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(25)))) shouldBe 25
      }
    }

    describe("Tree depth") {
      it ("should return 1 for leaf") {
        Tree.depthByFold(Leaf(1)) shouldBe 0
      }

      it ("should return depth for shallow tree") {
        Tree.depthByFold(Branch(Leaf(1), Leaf(2))) shouldBe 1
      }

      it ("should return depth for deep(er) tree") {
        Tree.depthByFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) shouldBe 2
      }
    }

    describe("Tree map") {
      it ("should map leaf") {
        Tree.mapByFold(Leaf(1)) (_ * 2) shouldBe Leaf(2)
      }

      it ("should map shallow tree") {
        Tree.mapByFold(Branch(Leaf(1), Leaf(2))) (_ * 2) shouldBe Branch(Leaf(2), Leaf(4))
      }

      it ("should map deep(er) tree") {
        Tree.mapByFold(Branch(Leaf(1), Branch(Leaf(4), Leaf(2)))) (_ * 2) shouldBe Branch(Leaf(2), Branch(Leaf(8), Leaf(4)))
      }
    }
  }
}
