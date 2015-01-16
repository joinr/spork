;an abstract set of functions for working on maps, treating them as 
;simple tag databases.  tags are categories applied to multiple 
;subjects.  Subjects are unique identifiers that are subjected to a tag.
;Tags are basically meta-data....except it's explicit.
(ns spork.util.tags
  (:require [spork.util [general :as gen]]))

(defprotocol ITagStore 
 (get-tags [store subject]  "Fetch any tags associated with a subject from database m.")
 (get-subjects [store tag]  "Fetch any subjects associated with a tag, from database m.")
 (add-tag [store tag] "Add a tag to the database.  Should require a subject to exist.")
 (add-subject [store subject])
 (drop-tag [store tag]  "Drops a tag from all subjects, removes from store.")
 (drop-subject [store subject] "Drops a subject from all tags, removes from store.")
 (tag-subject [store tag subject]   "Impose a tag on a subject.")
 (untag-subject [store tag subject]  "Remove a tag from a subject."))


(defmacro try-get [m k not-found]
  `(if-let [v# (get ~m ~k)]
     v#
     ~not-found))

(declare mutable-tags)

;;Generate a simple tag database implemented using maps.
;;tags correspond to simple attributes (usually keywords)
;;associated with a subject, where subjects are unique 
;;identifiers.  It's really just two tables with many->many
;;relationships.  Calling it a tag is more for semantic 
;;clarity...
(defrecord tags [tags subjects]
  ITagStore
  (get-tags [store subject]     (get subjects subject))
  (get-subjects [store tag]     (get tags tag))
  (add-tag [store tag]          (tags. (assoc tags tag #{}) subjects))
  (add-subject [store subject]  (tags. tags (assoc subjects subject #{})))
  (drop-tag [store tag]         (tags. (dissoc tags tag)                                        
                                       (reduce (fn [acc subj] 
                                                 (if-let [restags (disj (get acc subj) tag)]
                                                   (assoc acc subj restags)
                                                   (dissoc acc subj)))
                                               subjects 
                                               (get tags tag))))
  (drop-subject [store subject] (tags. (reduce (fn [acc tag] 
                                                 (if-let [ressubjs (disj (get acc tag) subject)]
                                                   (assoc acc tag ressubjs)
                                                   (dissoc acc tag)))
                                               tags 
                                               (get subjects subject))
                                       (dissoc subjects subject)))
  (tag-subject [store tag subject]   
    (let [oldt (try-get tags tag #{})
          olds (try-get subjects subject #{})]
      (tags. (assoc tags tag (conj oldt subject))
             (assoc subjects subject (conj olds tag)))))
  (untag-subject [store tag subject]
    (let [new-tags (disj (try-get tags tag #{}) subject)
          new-subjects (disj (try-get subjects subject #{}) tag)]
      (tags. (if (empty? new-tags)
               (dissoc tags tag)
               (assoc tags tag new-tags))
             (if (empty? new-subjects)
               (dissoc subjects subject)
               (assoc subjects subject new-subjects)))))    
  clojure.lang.IEditableCollection
  (asTransient [coll] (mutable-tags (gen/transient2 tags) (gen/transient2 subjects))))

;;We're generally not storing a ton of stuff in the tag entries.  Can
;;probabably pull this back a bit to use normal hashsets for the
;;actual entries.
(defrecord mtags [tags subjects]
  ITagStore
  (get-tags [store subject]     (get subjects subject))
  (get-subjects [store tag]     (get tags tag))
  (add-tag [store tag]          (mtags. (assoc! tags tag (transient #{})) subjects))
  (add-subject [store subject]  (mtags. tags (assoc! subjects subject (transient #{}))))
  (drop-tag [store tag]         (mtags. (dissoc! tags tag)                                        
                                       (reduce (fn [acc subj] 
                                                 (if-let [restags (disj! (get acc subj) tag)]
                                                   (assoc! acc subj restags)
                                                   (dissoc! acc subj)))
                                               subjects 
                                               (get tags tag))))
  (drop-subject [store subject] (mtags. (reduce (fn [acc tag] 
                                                 (if-let [ressubjs (disj! (get acc tag) subject)]
                                                   (assoc! acc tag ressubjs)
                                                   (dissoc! acc tag)))
                                               tags 
                                               (get subjects subject))
                                       (dissoc! subjects subject)))
  (tag-subject [store tag subject]   
    (let [oldt (try-get tags tag (transient #{}))
          olds (try-get subjects subject (transient #{}))]
      (mtags. (assoc! tags tag (conj! oldt subject))
              (assoc! subjects subject (conj! olds tag)))))
  (untag-subject [store tag subject]
    (let [new-tags (disj! (try-get tags tag (transient #{})) subject)
          new-subjects (disj! (try-get subjects subject (transient #{})) tag)]
      (mtags. (if (zero? (count new-tags))
               (dissoc! tags tag)
               (assoc! tags tag new-tags))
             (if (zero? (count new-subjects))
               (dissoc! subjects subject)
               (assoc! subjects subject new-subjects))))) 
  clojure.lang.ITransientCollection
  (persistent [coll]   (->tags (gen/persistent2! tags) (gen/persistent2! subjects)))
  (conj       [coll v] (.tag-subject coll (key v) (val v))))
  
(def empty-tags (tags. {} {}))
(defn mutable-tags [tgs subjcs] (mtags. tgs subjcs))

(definline subject->tags [tags subject] `(get-subjects ~tags ~subject))
(definline tag->subjects [tags tag] `(get-tags ~tag ~tag))

(defn has-tag?     [tags tag subject] (contains? (tag->subjects tags tag) subject))

(defn has-tags? [tags subject xs]
  (when-let [knowns (subject->tags tags subject)]
    (every? knowns  xs)))

(defn some-tags? [tags subject xs]
  (let [knowns (subject->tags tags subject)]
    (reduce (fn [acc x]
              (if (contains? knowns x)
                (reduced x)
                acc)) nil xs)))

(defn has-subject?  [tags tag subject] (has-tag? tags tag subject)) 
(defn has-subjects? [tags tag xs]
  (when-let [knowns (tag->subjects tags tag)]
    (every? knowns  xs)))

(defn some-subjects? [tags tag xs]
  (let [knowns (tag->subjects tags tag)]
    (reduce (fn [acc x]
              (if (contains? knowns x)
                (reduced x)
                acc)) nil xs)))

(defn and-tags
  "Select subjects that have every tag in xs."
  [m xs]
  (reduce #(clojure.set/intersection %1 (tag->subjects m %2)) #{} xs))

(defn or-tags
  "Select subjects that have any tag in xs."
  [m xs]
  (reduce #(clojure.set/union %1 (tag->subjects m %2)) #{} xs))

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

(def tagset [:old :fat :mean :onery :bald :goof])


 
)
