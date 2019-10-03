(ns spork.util.codecount
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [spork.util [io :as io]]))


;;Given a source file, let's break it down
;;by lines and parse the sequence of lines into
;;a structure of code blocks.
;;Our goal is to also recognize legacy
;;Mardown headers (via =====) and
;;to produce syntax-highlighted code
;;examples in org.

;;Specs for Code and Comments
;;===========================
;;We'll be leveraging regexes a lot, but
;;we don't "have" too...We could use the
;;token-based charseq parsing example, but
;;particularly if we wanted testable specs
;;or to generate random inputs from the specs,
;;but that's a bridge too far for now!

;;line beginning with zero or more whitespace
(def +comment+ #"s*;+")
(defn comment? [x]  (re-find +comment+  x))
(s/def ::commented-text  comment?)

(defn blank-line? [x]
  (or (= x "")
      (re-find #"^\s+$"  x)))

(s/def ::blank-line blank-line? )

;;anything between begin-line and \; is valid source
(def +non-comment+ #"^[^;]+")

;;Couldn't figure out a regex to ignore inside of
;;string literals when checking for the comment
;;token, so I had to hack this up.  Apologies!
(defn non-comment [^String x]
  (let [n (count x)]
    (loop [idx 0
           mode :outside]
      (if (== idx n)
        x
        (let [c (.charAt x idx) ]
          (if (= c \") ;skip string literals
            (case mode
              :outside (recur (unchecked-inc idx) :inside)
              (recur (unchecked-inc idx) :outside))
            (if (= c \;) ;;comment-break
              (if (and (= mode :outside)
                       (not= (.charAt x (dec idx)) \\ ))
                  (subs x 0 idx)
                  (recur (unchecked-inc idx) mode))
            (recur (unchecked-inc idx) mode))))))))

(defn non-comment? [x]
  (when-let [x  (re-find +non-comment+ x)]
    (not (blank-line? x))))

(s/def ::code
  (s/and non-comment?
         (s/conformer (fn [ln]
                        (let [body (non-comment ln)]
                        [:code 
                         {:open   (count (re-seq #"\("  body))
                          :closed (count (re-seq #"\)"  body))
                          :line ln}])))))
(s/def ::non-code
  (s/or :blank ::blank-line
        :comment ::commented-text))

(s/def ::source-block
   (s/or :code-line    ::code
         :non-code ::non-code))

(s/def ::source-code (s/* ::source-block))

;;Source Code Parsing Pipeline
;;============================
(defn source->tree [path]
  (->> (slurp path)
       (str/split-lines)
       (s/conform ::source-code)))

;;once we have our tagged tree, we can
;;process it to determine which lines belong to
;;code blocks, based on lexical "depth."

(defn tree->depth [xs]
  (let [d (atom 0)]
    (for [[cl [kind nd]] xs
          ]
      (case cl
        :code-line
        (let [{:keys [open closed]} nd
              delta (- open closed)
              dnew (swap! d + delta)]
          (merge {:kind :code-line :depth dnew} nd ))
        {:kind :non-code :depth @d :line nd}))))

;;Since we have depth now, should be able to group
;;our lines by depth, and classify them in the
;;process.
(defn depth-grouping [{:keys [depth kind] :as nd}]
  (cond (and (zero? depth) (= kind :non-code))
            :top-level-comment
        (> depth 0) :multi-line-form
        (and (zero? depth) (= (:open nd) (:closed nd)))
          :top-level-form
        :else  :multi-line-form ))

(defn tree->groupings [tr]
  (->> tr
       (tree->depth)
       (map-indexed (fn [idx nd] (assoc nd :idx idx :scope (depth-grouping nd))))
       (partition-by :scope)))


(defn analyze-source [path]
  (->> path
       (source->tree)
       (tree->groupings)
       (apply concat)))


(defn classify [fragment]
  (case (:kind fragment)
    :code-line :code
    (if (some->> fragment :line comment?)
      :comment
      :blank)))

(defn classify-file [path]
  (->> path
       analyze-source
       (map classify)
       frequencies))

(defn selective-file-seq
  "A tree seq on java.io.Files"
  ([dir file-filter]
   (tree-seq
    (fn [^java.io.File f] (. f (isDirectory)))
    (fn [^java.io.File d] (filter file-filter (seq (. d (listFiles)))))
    dir))
  ([dir] (selective-file-seq dir identity)))

(def exclusions #{".git"
                  "checkouts"
                  "target"})

(defn clojure-source? [path]
  (and (str/includes? path ".clj")
       (not (re-find #"\~|\#" path))))

(defn valid-folder? [path]
  (not (some (fn [s] (str/includes? path s)) exclusions)))

(defn classify-tree [root]
  (let [root-dir (io/as-directory root)]
    (->> (for [f (file-seq (io/file root-dir))
               :when  (and (clojure-source? (io/fpath f))
                           (valid-folder? (io/fpath f)))]
           [(clojure.string/replace (io/fpath f) root-dir "") (classify-file f)])
         (into {}))))

(defn classify-project [root]
  (let [pieces (classify-tree root)
        totals (reduce (partial merge-with +) {:code 0 :comment 0 :blank 0} (vals pieces))]
    (with-meta {:path   root
                :totals totals}
      {:pieces pieces})))

(defn classify-projects [& ps]
  (for [p ps]
    (classify-project p)))
