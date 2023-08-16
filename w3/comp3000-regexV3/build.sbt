/*
 * This file is part of COMP3000 tokens exercise.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

ThisBuild/organization := "comp.mq.edu.au"
ThisBuild/scalaVersion := "2.12.8"
ThisBuild/scalacOptions :=
    Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xcheckinit",
//        "-Xfatal-warnings",
//        "-Xlint:-stars-align,_"
    )

// Settings for the tokens exercise project.
lazy val tokens = (project in file("."))
  .settings(
    // Project information
    name := "Comp3000 tokens",
    version := "0.1",

    // Fork the JVM to run the doodle application. Then when the application
    // exits it won't kill the JVM that is running SBT.
    fork in run := true,

    // include API mappings
    autoAPIMappings := true,

    // Dependencies
    libraryDependencies ++=
      Seq (
        "junit" % "junit" % "4.12" % "test",
        "org.scalatest" %% "scalatest" % "3.0.8" % "test",
        "org.creativescala" %% "doodle" % "0.9.4"
      )
  )

// Interactive settings

logLevel := Level.Info

shellPrompt := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}


