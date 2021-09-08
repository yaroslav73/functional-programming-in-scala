package chapter03

import chapter03.datastructures.{Branch, Leaf, Tree}
import org.scalatest.wordspec.AnyWordSpec

class TreeSpec extends AnyWordSpec {
  "A Tree" when {
    "have 2 Leafs" should {
      "should have size 2" in {
        val tree = Branch(Leaf(0), Leaf(1))
        assert(Tree.fold(tree)(_ => 1)(_ + _) == 2)
      }
    }

    "have 4 Leafs" should {
      "should have size 4" in {
        val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
        assert(tree.size == 4)
      }
    }

    "Tree of (3, 1, 4, 2)" should {
      "maximum should be 4" in {
        val tree = Branch(Branch(Leaf(3), Leaf(1)), Branch(Leaf(4), Leaf(1)))
        assert(Tree.maximum(tree) == 4)
      }
    }

    "Tree has 5 Branches" should {
      "depth should be 5" in {
        val tree = Branch(
          Leaf(1), Branch(
            Branch(
              Leaf(2),
              Leaf(3)
            ),
            Branch(
              Branch(
                Leaf(5),
                Leaf(6)),
              Leaf(4)
            )
          )
        )

        assert(tree.depth == 5)
      }
    }

    "Tree has 2 Leafs" should {
      "depth should be 2" in {
        val tree = Branch(Leaf(1), Leaf(3))
        assert(tree.depth == 2)
      }
    }

    "only Leaf" should {
      "depth should be 1" in {
        val tree = Leaf('A')
        assert(tree.depth == 1)
      }
    }

    "have root and 2 branches" should {
      "have depth is 3" in {
        val tree = Branch(
          Branch(
            Leaf(2),
            Leaf(3)
          ),
          Branch(
            Leaf(5),
            Leaf(6)),
        )

        assert(Tree.fold(tree)(_ => 1)((l, r) => 1 + (l max r)) == 3)
      }
    }

    "apply map for increase leaf values by 1" should {
      "return tree with increases values by 1" in {
        val tree = Branch(
          Branch(
            Leaf(2),
            Leaf(3)
          ),
          Branch(
            Leaf(5),
            Leaf(6)),
        )

        val expected = Branch(
          Branch(
            Leaf(3),
            Leaf(4)
          ),
          Branch(
            Leaf(6),
            Leaf(7)),
        )

        assert(Tree.map(tree)(_ + 1) == expected)
      }
    }
  }
}
