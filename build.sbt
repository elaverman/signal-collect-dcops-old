import AssemblyKeys._ 
assemblySettings

/** Project */
name := "signal-collect-dcops"

version := "2.0.0-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.10.0-RC5"

resolvers += "Typesafe Snapshot Repository" at "http://repo.typesafe.com/typesafe/snapshots/"

/** Dependencies */
libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-library" % "2.10.0-RC5"  % "compile",
 "com.google.collections" % "google-collections" % "1.0" ,
 "ch.ethz.ganymed" % "ganymed-ssh2" % "build210"  % "compile",
 "commons-io" % "commons-io" % "2.4" ,
 "commons-codec" % "commons-codec" % "1.7"  % "compile",
 "junit" % "junit" % "4.8.2"  % "test",
 "org.specs2" % "specs2_2.10.0-RC5" % "1.12.3"  % "test",
 "net.sf.jung" % "jung2" % "2.0.1" % "pom",
 "net.sf.jung" % "jung-graph-impl" % "2.0.1",
 "net.sf.jung" % "jung-algorithms" % "2.0.1",
 "net.sf.jung" % "jung-visualization" % "2.0.1",
 "org.swinglabs" % "swing-layout" % "1.0.3"
  )