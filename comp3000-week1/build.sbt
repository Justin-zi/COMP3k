name := "comp332-week1"

version := "0.2"

organization := "comp.mq.edu.au"

// Scala compiler settings

scalaVersion := "2.12.8"

scalacOptions :=
    Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xcheckinit",
        "-Xfatal-warnings",
        "-Xlint:-stars-align,_"
    )

// Interactive settings

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

// Dependencies

libraryDependencies ++=
    Seq (
        "junit" % "junit" % "4.12" % "test",
        "org.scalatest" %% "scalatest" % "3.0.3" % "test"
    )
