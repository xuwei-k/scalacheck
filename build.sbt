sourceDirectory := file("dummy source directory")

scalaVersionSettings

// When bumping to 1.14.1, remember to set mimaPreviousArtifacts to 1.14.0
lazy val versionNumber = "1.14.0"

lazy val isRelease = false

lazy val travisCommit = Option(System.getenv().get("TRAVIS_COMMIT"))

lazy val scalaVersionSettings = Seq(
  scalaVersion := "2.12.0",
  crossScalaVersions := Seq("2.10.6", "2.11.8", scalaVersion.value)
)

lazy val sharedSettings = MimaSettings.settings ++ scalaVersionSettings ++ Seq(

  name := "scalacheck",

  version := {
    val suffix =
      if (isRelease) ""
      else travisCommit.map("-" + _.take(7)).getOrElse("") + "-SNAPSHOT"
    versionNumber + suffix
  },

  isSnapshot := !isRelease,

  organization := "org.scalacheck",

  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),

  homepage := Some(url("http://www.scalacheck.org")),

  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username, password
  )).toSeq,

  unmanagedSourceDirectories in Compile += (baseDirectory in LocalRootProject).value / "src" / "main" / "scala",

  unmanagedSourceDirectories in Test += (baseDirectory in LocalRootProject).value / "src" / "test" / "scala",

  resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases",

  javacOptions += "-Xmx1024M",

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

  scalacOptions in (Compile,doc) += "-Xfatal-warnings",

  //mimaPreviousArtifacts := (
  //  if (CrossVersion isScalaApiCompatible scalaVersion.value)
  //    Set("org.scalacheck" %%% "scalacheck" % "1.14.0")
  //  else
  //    Set.empty
  //),

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    val (name, path) = if (isSnapshot.value) ("snapshots", "content/repositories/snapshots")
                       else ("releases", "service/local/staging/deploy/maven2")
    Some(name at nexus + path)
  },

  publishMavenStyle := true,

  // Force clean before packaging.
  // https://github.com/rickynils/scalacheck/issues/318
  packageBin in Compile := {
    clean.value
    (packageBin in Compile).value
  },

  // Travis should only publish snapshots
  publishArtifact := !(isRelease && travisCommit.isDefined),

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
    scalaJSOptimizerOptions ~= { options =>
      // https://github.com/scala-js/scala-js/issues/2798
      try {
        scala.util.Properties.isJavaAtLeast("1.8")
        options
      } catch {
        case _: NumberFormatException =>
          options.withParallel(false)
      }
    },
    scalaJSStage in Global := FastOptStage,
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )
  .enablePlugins(ScalaJSPlugin)

lazy val jvm = project.in(file("jvm"))
  .settings(sharedSettings: _*)
  .settings(
    libraryDependencies += "org.scala-sbt" %  "test-interface" % "1.0"
  )
