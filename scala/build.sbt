scalaVersion := "2.11.6"
fork := true
javaOptions += "-Djava.library.path=./native/linux/x64"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2"
)
