resolvers += "scalatl" at "http://milessabin.com/scalatl"
//scalaVersion := "2.11.8",
scalaVersion := "2.11.8-tl-201604190743"

scalaBinaryVersion := "2.11"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.4.1"
  , compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"))

scalacOptions ++= Seq("-Yhigher-order-unification")
