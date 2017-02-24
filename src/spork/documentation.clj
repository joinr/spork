(ns spork.documentation
  (:require [clojure.java [shell :as shell]]))

;;#Documentation v0.1 - i.e. this file
(def documentation "spork.documentation")

;;#The standard prelude for all marathon docs.
;;Basically a bumper sticker namespace with summary info.
(def prelude ["marathon.prelude"])

(defn expand-paths [root xs] (map #(str root \. %) xs))
(defn path->file   [p]
  (str "src/" (clojure.string/replace p \. \/) ".clj"))

;;#Discrete Event Simulation Library.
(def simulation-lib
   ["sim.simcontext"
    "sim.pure.network"
    "sim.updates"
    "sim.agenda"
    "sim.data"])

;;#Graph Operations
;;In time, I will link to __cljgraph__, another library I built.
