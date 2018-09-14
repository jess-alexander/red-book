package scalapatterns.comprehensions

import org.scalatest.FlatSpec

class TestComprehensions extends FlatSpec {

  "Test" should "comprehensions" in {
    val userBase = List(new User("Travis", 28),
      new User("Kelly", 33),
      new User("Jennifer", 44),
      new User("Dennis", 23))

    val twentySomethings = for (
      user <- userBase
      if (user.age >=20 && user.age < 30)
    ) yield user.name  // i.e. add this to a list

    twentySomethings.foreach(name => println(name))  // prints Travis Dennis

  }
}