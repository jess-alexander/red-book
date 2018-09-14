val commonSettings = Seq(
  scalaVersion := "2.12.3",
  libraryDependencies ++= Seq(
    "org.scala-js" %% "scalajs-test-interface" % "0.6.14",
    "org.scalatest" %% "scalatest" % "3.0.1",
    "org.scalacheck" %% "scalacheck" % "1.14.0",
    "com.novocode" % "junit-interface" % "0.11",
    "org.scala-lang" % "scala-library" % scalaVersion.value
  )

)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "learning-scala"
  )
