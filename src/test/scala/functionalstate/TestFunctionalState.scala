package functionalstate

import org.scalatest.FlatSpec

class TestFunctionalState extends FlatSpec {

  trait RNG {
    def nextInt:(Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  def rollDie(a: scala.util.Random): Int = {
    a.nextInt(6)
  }

  class MockRandom extends scala.util.Random {
    override def nextInt(): Int = {
      100
    }
  }

  "Test" should "functional state issue" in {
    val random = new scala.util.Random()
    println(rollDie(new MockRandom()))
  }


  "Test" should "pure functional" in {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    println(n1)
    val (n2, rng3) = rng2.nextInt
    println(n2)
  }
}
