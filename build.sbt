name := "cs252_hw5"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq("-unchecked", "-deprecation")

retrieveManaged := true

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

EclipseKeys.withSource := true

EclipseKeys.configurations := Set(Compile)
