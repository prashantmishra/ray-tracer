name := "RayTracing"

scalaVersion := "3.7.1"

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.5.18",
  "org.scalafx" %% "scalafx" % "24.0.0-R35"
)

lazy val osName = "linux"

libraryDependencies ++= Seq(
  "org.openjfx" % s"javafx-controls" % "24.0.1" classifier osName,
  "org.openjfx" % s"javafx-graphics" % "24.0.1" classifier osName,
  "org.openjfx" % s"javafx-base" % "24.0.1" classifier osName,
  "org.openjfx" % s"javafx-media" % "24.0.1" classifier osName,
  "org.openjfx" % s"javafx-web" % "24.0.1" classifier osName,
  "org.openjfx" % s"javafx-swing" % "24.0.1" classifier osName
)