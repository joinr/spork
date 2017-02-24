;;minor wrapper/extensions around clojure.xml.
;;convenience functions.
(ns spork.util.xml
  (:require [clojure [xml :as xml]]))


(defn getbytes [instring] 
  (clojure.java.io/input-stream (.getBytes instring)))

(defn xparse
  "Parse an xml source.  Just a shadow for parse."
  [s] (xml/parse s))

(defn str->xml
  "Parse a string as if it were an xml source."
  [s]
  (xparse (getbytes s)))

(def xemit xml/emit) 
(def xemit-element xml/emit-element)
(def xtag xml/tag)
(def xcontent xml/content)
(def xattrs xml/attrs)

(defrecord xelm [tag attrs content])
(defn make-xelm
  ([tag attrs content]
    (cond 
      (or (nil? content)
          (vector? content))
          (xelm. tag attrs content)
      (or (map? content)
          (set? content)
          (not (seq? content)))
          (xelm. tag attrs [content])
      :true
        (xelm. tag attrs (apply vector content))))
  ([tag] (make-xelm tag nil nil))
  ([tag attrs] (make-xelm tag attrs nil)))

(defn spit-xml [x] (with-out-str (xemit x)))
