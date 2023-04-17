import Dependencies.*

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "uk.co.devproltd"
ThisBuild / organizationName := "DevPro Ltd"

lazy val root = (project in file("."))
  .settings(
    name := "checkout-system",
    libraryDependencies += scalatest % Test
  )
