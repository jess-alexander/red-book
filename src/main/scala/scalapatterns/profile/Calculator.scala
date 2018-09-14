package scalapatterns.profile

trait Calculator[A <: Profile] {
  def add(i: Int, j:Int): Int
}

object Calculator {

  implicit object prodCalculator extends Calculator[ProdProfile] {
    def add(i: Int, j: Int): Int = {
      i + j
    }
  }

  implicit object testCalculator extends Calculator[TestProfile] {
    def add(i: Int, j: Int): Int = {
      i
    }
  }
}

