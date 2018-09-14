package redbook.chapter2

import Chapter2._
import org.scalacheck.Gen
import org.scalatest.{FunSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class Chapter2Test extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  def provenFib(n: Int): Int = n match {
    case x if x < 1 => 0
    case 1 => 1
    case _ => provenFib(n - 1) + provenFib(n - 2)
  }

  def add(x: Int, y: Int): Int = {
    x + y
  }

  def curriedAdd(x: Int)(y: Int): Int = {
    x + y
  }

  def stringify(x: Int): String = {
    x.toString
  }

  def negate(x: Int): Int = {
    -x
  }

  describe("A tail recursive Fibonacci sequence function") {
    it("should for any positive n return the fibonacci number fib(n)") {
      forAll((Gen.chooseNum(1, 40), "fibonacciInput")) { n: Int => fib(n) shouldEqual provenFib(n) }
    }

    it("should return 0 for input of 0"){
      fib(0) shouldEqual 0
    }
    it("should return 1 for input of 1"){
      fib(1) shouldEqual 1
    }
    it("should return 1 for input of 2"){
      fib(2) shouldEqual 1
    }

  }

  describe("A sort checker") {
    it("should determine whether an integer list is sorted") {
      forAll { xs: List[Int] => isSorted(xs.toArray, (x: Int, y: Int) => x < y) shouldEqual (xs.sorted == xs) }
    }

    it("should return true when passed [1,2,3] and a>b"){
      isSorted(Array(1,2,3), (a:Int,b:Int)=>a<b) shouldEqual true
    }

    it("should return false when passed 3,2,1 and a>b"){
      isSorted(Array(3,2,1), (a:Int,b:Int)=>a<b) shouldEqual false
    }

  }

  describe("A curried function") {
    it("should provide the same output as the original function") {
      forAll {
        (x: Int, y: Int) => {
          add(x, y) shouldEqual curry(add)(x)(y)
        }
      }
    }
  }

  describe("An uncurried function") {
    it("should provide the same output as the original function") {
      forAll {
        (x: Int, y: Int) => {
          curriedAdd(x)(y) shouldEqual uncurry(curriedAdd)(x, y)
        }
      }
    }
  }

  describe("An composed function") {
    it("should provide the same output as calling both input functions sequentially in order") {
      forAll {
        x: Int => {
          stringify(negate(x)) shouldEqual compose(stringify, negate)(x)
        }
      }
    }
  }
}