val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "rand",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-laws" % "2.10.0" % Test,
      "org.typelevel" %% "shapeless3-deriving" % "3.4.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "discipline-munit" % "2.0.0-M3" % Test
    )
  )
