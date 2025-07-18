ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

val Http4sVersion = "1.0.0-M22"
val CirceVersion = "0.14.0"
libraryDependencies ++= Seq(
    "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
    "org.http4s"      %% "http4s-circe"        % Http4sVersion,
    "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
    "io.circe"        %% "circe-generic"       % CirceVersion,
)
lazy val root = (project in file("."))
  .settings(
    name := "untitled"
  )
