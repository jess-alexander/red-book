package redbook.chapter3

import redbook.chapter3.List._
import org.scalacheck.Gen
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ListTest extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("Exercise 3.1: trickymatch") {
    def trickymatch =
      List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }

    it("should equal ???") {
      trickymatch shouldEqual 3
    }
  }

  describe("Exercise 3.2: tail") {
    it("should return an empty list for an empty list") {
      tail(List()) shouldEqual Nil
    }

    it("should return an empty list for a list of one element") {
      forAll { x: Int =>
        tail(List(x)) shouldEqual Nil
      }
    }

    it("should return the list after the first element") {
      forAll { (x: Int, y: Int, z: Int) =>
        tail(List(x, y, z)) shouldEqual List(y, z)
      }
    }
  }

  describe("Exercise 3.3: setHead") {
    it("should return a list containing only the new head for an empty list") {
      forAll { expected: Int =>
        setHead(List(), expected) shouldEqual List(expected)
      }
    }

    it("should return a list with the specified element for a list of one element") {
      forAll { (x: Int, expected: Int) =>
        setHead(List(x), expected) shouldEqual List(expected)
      }
    }

    it("should return the list with the first element replaced") {
      forAll { (x: Int, y: Int, z: Int, expected: Int) =>
        setHead(List(x, y, z), expected) shouldEqual List(expected, y, z)
      }
    }
  }

  describe("Exercise 3.4: drop") {
    it("should return an empty list for an empty list") {
      forAll { dropCount: Int =>
        drop(List(), dropCount) shouldEqual Nil
      }
    }

    it("should return the list with only the last element when `size - 1` is dropped") {
      forAll { xs: scala.List[Int] =>
        drop(List(xs: _*), xs.size - 1) shouldEqual xs.lastOption.fold[List[Int]](List())(List(_))
      }
    }
  }

  describe("Exercise 3.5: dropWhile") {
    def isEven(a: Int): Boolean = a % 2 == 0

    it("should return an empty list for an empty list") {
      dropWhile(List(), isEven) shouldEqual Nil
    }

    it("should return the list with the first matching elements dropped") {
      dropWhile(List(0, 2, 4, 3, 6, 0), isEven) shouldEqual List(3, 6, 0)
    }
  }

  describe("Exercise 3.6: init") {
    it("should return an empty list for an empty list") {
      init(List()) shouldEqual Nil
    }

    it("should return an empty list for a list of one element") {
      forAll { x: Int =>
        init(List(x)) shouldEqual Nil
      }
    }

    it("should return the list before the last element") {
      forAll { (x: Int, y: Int, z: Int) =>
        init(List(x, y, z)) shouldEqual List(x, y)
      }
    }
  }

  /* Exercise 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and
   * return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
   * might work if you call foldRight with a large list. This is a deeper question that we’ll
   * return to in chapter 5.
   */

  describe("Exercise 3.8: direct call to foldRight") {
    it("should equal ???") {
      // The type annotation Nil:List[Int] is needed here, because
      // otherwise Scala infers the B type parameter in foldRight as
      // List[Nothing].
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) shouldEqual List(1, List(2,List(3)))
    }
  }

  describe("Exercise 3.9: length (implemented using foldRight)") {
    it("should return zero for an empty list") {
      List.length(List()) shouldEqual 0
    }

    it("should return the correct length") {
      forAll { xs: scala.List[Int] =>
        List.length(List(xs: _*)) shouldEqual xs.size
      }
    }
  }

  describe("Exercise 3.10: foldRight") {
    it("should return the starting value for an empty list") {
      forAll { expected: Int =>
        foldRight(List[Int](), expected)(_ + _) shouldEqual expected
      }
    }

    it("should return the sum of any list, plus starting value") {
      forAll { (xs: scala.List[Int], starting: Int) =>
        foldRight(List(xs: _*), starting)(_ + _) shouldEqual xs.sum + starting
      }
    }
  }

  describe("Exercise 3.11") {
    describe("sumLeft (implemented using foldLeft)") {
      it("should return the zero for an empty list") {
        sumLeft(List[Int]()) shouldEqual 0
      }

      it("should return the sum of any list") {
        forAll { xs: scala.List[Int] =>
          sumLeft(List(xs: _*)) shouldEqual xs.sum
        }
      }
    }
    
    describe("productLeft (implemented using foldLeft)") {
      it("should return the one for an empty list") {
        productLeft(List[Double]()) shouldEqual 1d
      }

      it("should return the sum of any list") {
        forAll { xs: scala.List[Double] =>
          productLeft(List(xs: _*)) shouldEqual xs.product
        }
      }
    }

    describe("length (implemented using foldLeft)") {
      it("should return zero for an empty list") {
        lengthLeft(List()) shouldEqual 0
      }

      it("should return the correct length") {
        forAll { xs: scala.List[Int] =>
          lengthLeft(List(xs: _*)) shouldEqual xs.size
        }
      }
    }
  }

  describe("Exercise 3.12: reverse") {
    it("should return an empty list for an empty list") {
      reverse(List()) shouldEqual Nil
    }

    it("should return the list in reverse order") {
      forAll { xs: scala.List[Int] =>
        reverse(List(xs: _*)) shouldEqual List(xs.reverse: _*)
      }
    }
  }

  /* Exercise 3.13:
   * Hard: Can you write foldLeft in terms of foldRight? How about the other way
   * around? Implementing foldRight via foldLeft is useful because it lets us implement
   * foldRight tail-recursively, which means it works even for large lists without overflowing
   * the stack.
   */


  describe("Exercise 3.14: append") {
    it("should return the second list for an empty list") {
      forAll { xs: scala.List[Int] =>
        appendFold(List(), List(xs: _*)) shouldEqual List(xs: _*)
      }
    }

    it("should return the two lists concatenated") {
      forAll { (xs: scala.List[Int], ys: scala.List[Int]) =>
        appendFold(List(xs: _*), List(ys: _*)) shouldEqual append(List(xs: _*), List(ys: _*))
      }
    }
  }

  describe("Exercise 3.15: flatten") {
    it("should return an empty list for an empty list") {
      flatten(List[List[Int]]()) shouldEqual Nil
    }

    it("should return the two lists concatenated") {
      forAll { (xs: scala.List[scala.List[Int]]) =>
        flatten(List(xs.map(List(_: _*)): _*)) shouldEqual List(xs.flatMap(x => x): _*)
      }
    }
  }

}
