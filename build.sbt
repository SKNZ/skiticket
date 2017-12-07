import Dependencies._

lazy val root = (project in file(".")).
        settings(
            inThisBuild(List(
                organization := "net.sec.nfc",
                scalaVersion := "2.12.3",
                version := "0.1.0-SNAPSHOT"
            )),
            name := "SkiTicket",
            libraryDependencies += scalaTest % Test,
            libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
            libraryDependencies += "org.scodec" %% "scodec-core" % "1.10.3"
        )
