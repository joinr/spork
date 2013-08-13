(defproject spork/stand-alone "0.1.0-SNAPSHOT"
  :description
  "A set of libraries derived from Spoon's Operations Research Kit.
   Libraries are modular and are suppoorted as stand-alone dependencies.
   This project is an uberjar that includes all of the modular bits in a single 
   jar, for easier dependencies.  spork is purpose-built to be clojure-centric, 
   focusing on a functional API, a default towards purity and persistent data 
   structures, with alternative mutable/imperative counterparts.  The vast
   majority of spork is written in Clojure, with an intentional nod toward 
   independence on external libraries, particularly Java libs.  Consequently, 
   spork should provide a lightweight, clojure-based platform for the topics 
   covered by the libraries."
  :url "None Currently"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [spork/spork-core  "0.1.0-SNAPSHOT"]
                 [spork/spork-util  "0.1.0-SNAPSHOT"]
                 [spork/spork-sim   "0.1.0-SNAPSHOT"]
                 [spork/spork-opt   "0.1.0-SNAPSHOT"]
                 [spork/spork-graph "0.1.0-SNAPSHOT"]
                 [spork/spork-gui   "0.1.0-SNAPSHOT"]
                 [spork/spork-incanter "0.1.0-SNAPSHOT"]])
