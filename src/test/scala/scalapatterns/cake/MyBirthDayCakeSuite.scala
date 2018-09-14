package scalapatterns.cake

import scalapatterns.cake.MyBirthDayCake.{BananaCake, CheeseCake, CoconutCake, MainTrait}
import org.scalatest._

class MyBirthDayCakeSuite extends FlatSpec {

  "Cake Pattern" should "be" in {
    val p1 = new MainTrait with BananaCake
    println("Dependency Injection via interface~~~ " + p1.cook)

    val p2 = new MainTrait with CheeseCake
    println("Dependency Injection via interface~~~ " + p2.cook)

    val p3 = new MainTrait with CoconutCake
    println("Dependency Injection via interface~~~ " + p3.cook)
  }
}
