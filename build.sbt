import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

lazy val sharedSettings = mimaDefaultSettings ++ Seq(

  name := "scalacheck",

  version := "1.12.3-SNAPSHOT",

  organization := "org.scalacheck",

  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),

  homepage := Some(url("http://www.scalacheck.org")),

  scalaVersion := "2.11.5",

  crossScalaVersions := Seq("2.10.4", "2.11.5"),

  previousArtifact := Some("org.scalacheck" % "scalacheck_2.11" % "1.12.1"),

  unmanagedSourceDirectories in Compile += baseDirectory.value / "src-shared" / "main" / "scala",

  unmanagedSourceDirectories in Test += baseDirectory.value / "src-shared" / "test" / "scala",

  resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases",

  libraryDependencies ++= {
    if (scalaVersion.value startsWith "2.10") Seq.empty
    else Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")
  },

  javacOptions += "-Xmx1024M",

  scalacOptions ++= Seq("-deprecation", "-feature"),

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    val (name, path) = if (isSnapshot.value) ("snapshots", "content/repositories/snapshots")
                       else ("releases", "service/local/staging/deploy/maven2")
    Some(name at nexus + path)
  },

  publishMavenStyle := true,

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := {
    <scm>
      <url>https://github.com/rickynils/scalacheck</url>
      <connection>scm:git:git@github.com:rickynils/scalacheck.git</connection>
    </scm>
    <developers>
      <developer>
        <id>rickynils</id>
        <name>Rickard Nilsson</name>
      </developer>
    </developers>
  }
)

lazy val js = project.in(file("js"))
  .settings(sharedSettings: _*)
  .settings(
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )
  .enablePlugins(ScalaJSPlugin)

lazy val jvm = project.in(file("jvm"))
  .settings(sharedSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-sbt" %  "test-interface" % "1.0"
     ,"com.nicta" %% "rng" % "1.3.0"
    )
  )
