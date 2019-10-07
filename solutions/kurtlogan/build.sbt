import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "kurtlogan",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "helicoptor",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "dev.zio" %% "zio" % "1.0.0-RC12-1",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "com.softwaremill.quicklens" %% "quicklens" % "1.4.12",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % "0.11.1"),
    libraryDependencies += scalaTest % Test
  )
