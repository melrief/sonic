import sbt.Keys.crossScalaVersions

lazy val sonic =
  (project in file("."))
    .aggregate(core)
    .settings(commonSettings)
    .settings(publishArtifact := false)

lazy val core =
  (project in file("core"))
    .settings(commonSettings)

lazy val example =
  (project in file("example"))
    .settings(commonSettings)
    .dependsOn(core)


lazy val commonSettings = Seq(
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.11.8", "2.12.1"),
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-deprecation",
    "-Xfuture",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused"
  )
)
