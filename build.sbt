import Dependencies._

lazy val root = (project in file(".")).
        settings(
            inThisBuild(List(
                organization := "net.sec.nfc",
                scalaVersion := "2.12.4",
                version := "0.1.0-SNAPSHOT",
                parallelExecution := false
            )),
            concurrentRestrictions in Global += Tags.limit(Tags.Test, 1),
            name := "SkiTicket",
            libraryDependencies += scalaTest % Test,
            libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
            libraryDependencies += "org.scodec" %% "scodec-core" % "1.10.3",
            libraryDependencies ++= Seq(
                "org.scalikejdbc" %% "scalikejdbc" % "3.1.0",
                "com.h2database" % "h2" % "1.4.196",
                "ch.qos.logback" % "logback-classic" % "1.2.3"
            )
        )

fork in run := true
