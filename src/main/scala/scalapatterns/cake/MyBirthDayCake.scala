package scalapatterns.cake

object MyBirthDayCake {
  trait GenericBirthDayCake {
    def cook: String
  }

  trait BananaCake extends GenericBirthDayCake {
    def cook: String = "Cooking Banana Cake!!"
  }

  trait CheeseCake extends GenericBirthDayCake {
    def cook: String = "Cooking Cheese Cake!!"
  }

  trait CoconutCake extends GenericBirthDayCake {
    def cook: String = "Cooking Coconut Cake!!"
  }

  trait MainTrait {
    this: GenericBirthDayCake => println("Surprise ~~" + this.cook)
  }
}
