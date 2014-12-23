import SonatypeKeys._

sonatypeSettings

name := "textlib"

organization := "com.ad_dice.textlib"

profileName := "com.ad_dice"

version := "0.3.4"

scalaVersion := "2.11.2"

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/ad-dice/textlib</url>
  <licenses>
  <license>
    <name>Apache License, Version 2.0</name>
    <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
    <distribution>repo</distribution>
    <comments>A business-friendly OSS license</comments>
  </license>
</licenses>
  <scm>
    <url>git@github.com:ad-dice/textlib.git</url>
    <connection>scm:git:git@github.com:ad-dice/textlib.git</connection>
  </scm>
  <developers>
    <developer>
      <id>ad-dice</id>
      <name>Ito, Bya, Yoshida</name>
      <url>http://ad-dice.com/</url>
    </developer>
  </developers>)
