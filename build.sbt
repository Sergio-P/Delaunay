name := "Delaunay Triangulazation"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.92-R10"

mainClass in (Compile, run) := Some("cl.spenafiel.delaunay.VisuWindow")