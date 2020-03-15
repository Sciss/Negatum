# Negatum

[![Build Status](https://travis-ci.org/Sciss/Negatum.svg?branch=master)](https://travis-ci.org/Sciss/Negatum)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/negatum-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/negatum-core_2.13)

## statement

Negatum started as a genetic programming experiment for SoundProcesses,
and eventually became entangled with the particular sound installation of
the same name that was part of the exhibition "Imperfect Reconstruction".
In the future, its extensions to SoundProcesses will become part of
SoundProcesses/Mellite.

This project is (C)opyright 2016&ndash;2020 by Hanns Holger Rutz. All rights reserved.
It is released under the [GNU Affero General Public License](https://git.iem.at/sciss/Negatum/raw/master/LICENSE) v3+
and comes with absolutely no warranties. 
To contact the author, send an email to `contact at sciss.de`

## requirements / installation

This project builds against Scala 2.13, 2.12 using sbt (the last version to support Scala 2.11 was 0.8.1).

To build the application:

    sbt negatum-app/assembly
    
Then to run:

    java -jar app/Negatum.jar

## project structure

The project is now a hybrid between Mellite "extensions" and the original sound piece, with the
following sbt modules in place:

 - `negatum-core`: contains the SoundProcesses based objects for genetic programming (`Negatum`)
 - `negatum-views`: contains the Mellite views for core (e.g. `NegatumView`)
 - `negatum-app`: contains a standalone application with the original sound piece 
 
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

The current version `v` is `"0.13.0"`

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)
