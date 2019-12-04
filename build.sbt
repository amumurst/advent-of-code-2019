scalaVersion := "2.13.1"
name := "advent-of-code-2019"
enablePlugins(GraalVMNativeImagePlugin)

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

graalVMNativeImageOptions ++= Seq(
  "--initialize-at-build-time",
  "--initialize-at-run-time=" +
    "scala.util.Random",
  "--no-fallback",
)