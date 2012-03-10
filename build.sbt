name := "cs252_hw5"

scalaSource in Compile <<= baseDirectory(_ / "src")

javaSource in Compile <<= baseDirectory(_ / "src")

scalacOptions ++= Seq("-unchecked", "-deprecation")

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

EclipseKeys.withSource := true

EclipseKeys.configurations := Set(Compile)
