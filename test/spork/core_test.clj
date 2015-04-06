(ns spork.core-test
  (:use clojure.test)
  (:require [spork.data.test]
            [spork.cljgraph.tests]))

(run-tests 'spork.data.test 'spork.cljgraph.tests)
