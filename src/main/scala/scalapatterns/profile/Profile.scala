package scalapatterns.profile

sealed abstract class Profile
sealed abstract class ProdProfile extends Profile
sealed abstract class TestProfile extends Profile