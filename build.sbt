name := "Geometry"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.ow2.asm" % "asm-all" % "5.0.3"
)
