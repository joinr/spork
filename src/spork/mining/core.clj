(ns spork.mining.core 
  (:require [spork.util [table :as tbl]]))

(defn find-spork []
  (let [paths (->> (clojure.string/split 
                     (java.lang.System/getProperty "java.class.path")
                     #";")
                  (filter #(.contains % "spork")))]
    (if-let [jar-file (some #(when (.contains % ".jar") %) paths)]
      jar-file
      (some #(when (.contains % "\\src") %) paths))))

(defn find-spork-data []
  (let [path (find-spork)]
    (if (.contains path ".jar")
        (throw (Exception. "dunno how to get resources in jar yet"))
        path)))
  
(def datasets {:blog "\\spork\\mining\\sampledata\\blogdata.txt"
               :addresslist "\\spork\\mining\\sampledata\\addresslist.txt"
               :agesonly    "\\spork\\mining\\sampledata\\agesonly.csv"
               :articles "\\spork\\mining\\sampledata\\articles.txt"
               :features "\\spork\\mining\\sampledata\\features.txt"
               :feedlist "\\spork\\mining\\sampledata\\feedlist.txt"
               :schedule "\\spork\\mining\\sampledata\\schedule.txt"
               :matchmaker "\\spork\\mining\\sampledata\\matchmaker.csv"
               :stockfeatures "\\spork\\mining\\sampledata\\stockfeatures.txt"
               :zebo "\\spork\\mining\\sampledata\\zebo.txt"})
        
(defn get-dataset
  "Fetches a keyed dataset of embedded sample data.  Returns a string of the 
   encoded data.  Cleaning and formatting is up to the caller!."
  [datakey & {:keys [datapath]
                              :or   {datapath (find-spork-data)}}]
  (if-let [path (get datasets datakey)]
    (slurp (str datapath path))))
  
  