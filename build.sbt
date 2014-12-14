name := "Geometry"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.ow2.asm" % "asm-all" % "5.0.3"
)
