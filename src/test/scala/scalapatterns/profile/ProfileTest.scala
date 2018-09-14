package scalapatterns.profile
import org.scalatest.FlatSpec

class ProfileTest extends FlatSpec{
  "Profile" should "verify Prod Profile" in {
    assert(implicitly[Calculator[ProdProfile]].add(1,2) == 3)
  }

  it should "verify Test Profile" in {
    assert(implicitly[Calculator[TestProfile]].add(1,2) == 1)
  }
}


