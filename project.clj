(defproject spork "0.1.9.3-SNAPSHOT"
  :description
  "A set of libraries derived from Spoon's Operations Research Kit.
   Libraries are modular and are suppoorted as stand-alone dependencies.
   This project is an uberjar that includes all of the modular bits in a single
   jar, for easier dependencies.  spork is purpose-built to be clojure-centric,
   focusing on a functional API, a default towards purity and persistent data
   structures, with alternative mutable/imperative counterparts.  The vast
   majority of spork is written in Clojure, with an intentional nod toward
   independence from external libraries, particularly Java libs.  Consequently,
   spork should provide a lightweight, clojure-based platform for the topics
   covered by the libraries.  I am currently working to eliminate legacy
   java dependencies where possible."
  :url "None Currently"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [clj-tuple "0.1.5"]
                 [immutable-int-map "0.1.0"]
                 [net.sf.jung/jung-api "2.0.1"]
                 [net.sf.jung/jung-graph-impl "2.0.1"]
                 [net.sf.jung/jung-algorithms "2.0.1"]
                 [net.sf.jung/jung-visualization "2.0.1"]
                 [net.sf.jung/jung-io "2.0.1"]
                 [dk.ative/docjure    "1.6.0-SNAPSHOT"]
                 [primitive-math "0.1.3"]
                 [org.clojure/core.match "0.2.1"]]
  :jvm-opts ^:replace ["-Xmx500m" "-XX:NewSize=200m"])
