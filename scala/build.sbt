lazy val root = project
  .in(file("."))
  .settings(
    name := "thesis",
    scalaVersion := "3.1.2-RC1",
    version := "0.1.0-SNAPSHOT",
  )
  .dependsOn(regex)

lazy val regex = project
  .enablePlugins(JmhPlugin)
  .settings(
    name := "thesis",
    scalaVersion := "3.1.2-RC1",
    version := "0.1.0-SNAPSHOT",
  )
