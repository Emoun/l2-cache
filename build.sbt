scalaVersion := "2.13.10"

// library name
name := "l2-cache"

Compile / unmanagedSourceDirectories += baseDirectory.value / "../soc-comm/src"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
)

val chiselVersion = "3.5.6"
addCompilerPlugin("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.6"

// For FIFO buffers
libraryDependencies += "edu.berkeley.cs" % "ip-contributions" % "0.5.4"
libraryDependencies += "com.fazecast" % "jSerialComm" % "[2.0.0,3.0.0)"

lazy val l2_cache = project in file(".")
