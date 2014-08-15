;an abstract set of functions for working on maps, treating them as 
;simple tag databases.  tags are categories applied to multiple 
;subjects.  Subjects are unique identifiers that are subjected to a tag.
;Tags are basically meta-data....except it's explicit.
(ns spork.util.tags
  (:require [spork.util [general :as gen]]))

;(defprotocol ITagStore 
;  (get-tags [store subject])
;  (get-subjects [store tag])
;  (add-tag [store tag])
;  (add-subject [store subject])
;  (drop-tag [store tag])
;  (drop-subject [store subject]))

(defn ->tags
  "Generate a simple tag database implemented using maps.
   tags correspond to simple attributes (usually keywords)
   associated with a subject, where subjects are unique 
   identifiers.  It's really just two tables with many->many
   relationships.  Calling it a tag is more for semantic 
   clarity..."
  [tags subjects]  {:tags tags  :subjects subjects})

(def empty-tags (->tags {} {}))

(defn get-tags
  "Fetch any tags associated with a subject from database m."
  [m subject] 
  (get-in m [:subjects subject]))

(defn has-tag? [tags tag subject] (contains? (get-tags tags subject) tag))
;(defn has-subject? [tags tag subject] (has-tag? tags tag subject)) 

(defn get-subjects
  "Fetch any subjects associated with a tag, from database m."
  [m tag] (get-in m [:tags tag]))

(defn add-tag
  "Add a tag to the database.  Should require a subject to exist."
  [m tag] (gen/deep-assoc m [:tags tag] #{}))

(defn and-tags
  "Select subjects that have every tag in xs."
  [m xs]
  (reduce #(clojure.set/intersection %1 (get-subjects m %2))) #{} xs)

(defn or-tags
  "Select subjects that have any tag in xs."
  [m xs]
  (reduce #(clojure.set/union %1 (get-subjects m %2))) #{} xs)

(defn add-subject [m subject] (gen/deep-assoc m [:subjects subject] #{}))

(defn tag-subject
  "Impose a tag on a subject."
  [m subject tag]
  (let [new-tags (conj (get-in m [:tags tag] #{}) subject) 
        new-subjects (conj (get-in m [:subjects subject] #{}) tag)]
  (-> (gen/deep-assoc m [:tags tag] new-tags)  
      (gen/deep-assoc [:subjects subject]  new-subjects))))

(defn untag-subject
  "Drop a tag from a subject."
  [m subject tag]
  (let [subjects (:subjects m)
        tags (:tags m)
        new-tags (disj (get tags tag #{}) subject) 
        new-subjects (disj (get subjects subject #{}) tag)]
    (->tags (if (empty? new-tags)
              (dissoc tags tag)
              (assoc tags tag new-tags))
            (if (empty? new-subjects)
              (dissoc subjects subject)
              (assoc subjects subject new-subjects)))))

(defn multi-tag
  "Impose many tags on a subject."
  [m subject tags]
  (reduce #(tag-subject %1 subject %2 ) m tags))

(defn multi-untag
  "Remove multiple tags from a subject."
  [m subject tags]
  (reduce #(untag-subject %1 subject  %2 ) m tags))

(comment ;testing 
  (def simple-tags 
    (-> empty-tags
      (multi-tag "tom" [:has-name :male])))
  (def dropped-tags 
    (-> simple-tags 
      (untag-subject "tom" :has-name)))
  
)
