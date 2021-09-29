name := "algebra-weighted-search"

lazy val interview = (project in file(".")).settings(coreSettings : _*)

lazy val commonSettings: Seq[Setting[_]] = Seq(
  organization := "net.debasishg",
  version := "0.01",
  scalaVersion := "2.13.6",

  scalacOptions in Compile ++= Seq( "-unchecked", "-feature", "-language:postfixOps", "-deprecation" ),
  resolvers += Resolver.sonatypeRepo("snapshots")
)

lazy val coreSettings = commonSettings ++ Seq(
  name := "algebra-weighted-search",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"   % "2.6.1",
    "org.typelevel" %% "cats-free"   % "2.6.1",
    "org.typelevel" %% "cats-effect" % "3.2.9",
    "org.typelevel" %% "spire" % "0.17.0",
    "com.higher-order" %% "mset" % "0.7.1",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    kindProjector
  )
)

val kindProjector = compilerPlugin(
  "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
)