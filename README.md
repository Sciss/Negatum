# Negatum

[![Build Status](https://github.com/Sciss/Negatum/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/Negatum/actions?query=workflow%3A%22Scala+CI%22)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/negatum-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/negatum-core_2.13)

## statement

Negatum started as a genetic programming experiment for SoundProcesses,
and eventually became entangled with the particular sound installation of
the same name that was part of the exhibition "Imperfect Reconstruction".
Now it is part of SoundProcesses/Mellite, with the original sound installation
remaining in a dedicated `app` project.

This project is (C)opyright 2016&ndash;2021 by Hanns Holger Rutz. All rights reserved.
It is released under the [GNU Affero General Public License](https://git.iem.at/sciss/Negatum/raw/main/LICENSE) v3+
and comes with absolutely no warranties. 
To contact the author, send an e-mail to `contact at sciss.de`.

## requirements / installation

This project builds against Scala 2.13, 2.12 using sbt (the last version to support Scala 2.11 was 0.8.1).

To build the application:

    cd app
    sbt assembly
    
Then to run:

    java -jar Negatum.jar

## project structure

The project is a hybrid between Mellite "extensions" and the original sound piece, with the
following sbt modules in place:

 - `negatum-core`: contains the SoundProcesses based objects for genetic programming (`Negatum`)
 - `negatum-views`: contains the Mellite views for `core` (e.g. `NegatumView`)
 - `negatum-app`: (in separate directory `app`) contains a standalone application with the original sound piece 
 
In the future, more abstractions (SOM, SVM) will be moved to the core module.
The dependency structure is now a bit tricky:

 - `negatum-core` depends on SoundProcesses
 - `negatum-views` depends on `negatum-core` and `mellite-core`
 - `mellite` (full) depends on `negatum-views`
 - `negatum` (full) depends on `mellite` (full)
 
So when building with locally published artifacts, the build/publish order is:

 1. `mellite-core`
 2. `negatum-views`
 3. `mellite` (full)
 4. `negatum` (full)

## linking

To use this project as a library, use the following artifact:

    libraryDependencies += "de.sciss" %% "negatum-core"  % v
    libraryDependencies += "de.sciss" %% "negatum-views" % v

The current version `v` is `"1.6.0"`

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)
