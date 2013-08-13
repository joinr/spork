(ns spork.entitysystem.entitytesting
  (:use [entitysystem.store]
        [clojure.pprint :only [pprint]]))

;; Examples/Tests....
(defcomponent coords [xy] xy)
(defcomponent visage [description] description)

(defentity simple-entity [id]
  [visage (str "The remnant of a lost age, standing alone against the evil that"
               " plagues this land...")
   coords [0 0]
   :playertag :human])

;Defines an entity that inherits components from the simple-entity
(defentity complex-entity [id]
  [simple-entity] 
  [visage (str "A much more complicated individual...")
   :playertag :robot
   :goals ['destroy-player]])

(defentity complex-entity [id]
  [simple-entity] [])


(defentity named-entity [name]
  [simple-entity]
  [visage (str "An entity named " name)
   :uniquename name])

(defentity flying-pig [id]
  [:nick "pot bellied terror" 
   :aged 100 
   :hitpoints 2000
   :locomotion 'flyingmotion
   :temperament :angry!])

(defn new-player [playername playercount]
  "Defines a new player, extending our simple-entity player spec with an
   additional playernumber component.  Note, playernumber is defined inline.  
   Components are simple data records."
  (conj-component 
    (simple-entity playername) 
    (->component :playernumber playercount)))

(defentity new-player [playercount]
  [simple-entity]
  [:playernumber playercount])

(def samplestore 
  (->>
    (conj (map-indexed (fn [idx n] (new-player n idx)) ["tom", "bob"]) 
          (complex-entity "Complicated Entity"))
       (add-entities emptystore)))

;execute some queries on the store...
(defn list-players
  "List all entities in ascending order of playernumber."
  [& {:keys [store] :or {store samplestore}}]
  (pprint 
    (select-entities store
       :from [:playernumber]
       :order-by (fn [e] (->> e :components :playernumber :data)))))


