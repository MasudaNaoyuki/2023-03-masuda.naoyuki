name := "tp-crypto-clefia"
//name := "tp-crypto-clefia"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

initialCommands in console := """import clefia._"""
