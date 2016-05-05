resolvers += "scalatl" at "http://milessabin.com/scalatl"
//scalaVersion := "2.11.8",
scalaVersion := "2.11.8"

scalaBinaryVersion := "2.11"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats" % "0.5.0"
  , compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full)
  , compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"))
