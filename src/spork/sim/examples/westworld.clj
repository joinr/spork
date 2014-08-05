;;This is an implementation of the WestWorld simulation
;;in Programming Game AI by Example, by Mat Buckland.
;;It should demonstrate defining a simple entity 
;;simulation that logs simulation output to standard
;;out.
(ns spork.sim.examples.westworld
  (:require [spork.sim [simcontext :as context]
                       [data :as sim]]
            [spork.entitysystem [store :as store :refer [defentity 
                                                         defcomponent]]]))
(defmacro defcomponents [names] 
  `(do 
     ~@(for [n names]
         `(store/defcomponent ~n [x#] x#))
     [~@(map (fn [symb] `(quote ~symb)) names)]))

(def ^:constant +max-gold+ 30)
(def ^:constant +max-fatigue+ 5)

(defcomponent stats [{:keys [location state] :as statmap}] statmap)
;;simple components 
(defcomponents 
  [money-in-bank ;how much money is deposited [integer]
   gold-carried  ;how much gold is in the entity's pocket [integer]
   fatigue       ;how fatigued the entity is [integer]
   thirst        ;how thirsty the entity is [integer]
   update-behavior ;how the entity updates itself when told to
                   ;[keyword or fn]
   ])

;;A simple container for our states.
(defn ->state [name enter execute exit] 
  {:name name :enter enter :execute execute :exit exit})
(defn change-state [estore id new-state]
  (let [e (store/get-entity estore id)
        old-state (:state e)
        exit (:exit old-state)
        enter (:enter new-state)]
    (->> e 
        (exit)
        (enter)
        (store/add-entity estore)))) 

;;documentation of the states the miner can be in.
(def states {:miner [:enter-mine-and-dig-for-nugget
                     :visit-bank-and-deposit-gold
                     :go-home-and-sleep-til-rested
                     :quench-thirst]
             :else []})

(defmacro defstate 
  ([name args {:keys [enter exit execute]
               :or {enter identity exit identity execute identity}}]
    `(defn ~name [~@args] 
       (->state ~(keyword name) ~enter ~execute ~exit)))
  ([name {:keys [enter exit execute]
               :or {enter identity exit identity execute identity}}]
    `(def ~name
       (->state ~(keyword name) ~enter ~execute ~exit))))

(defmacro entlog [name msg & expr] 
  `(do (println (str ~name ":" ~msg))
       ~@expr))

(defmacro with-components [[e cs] & expr]
  `(let [{:keys [~@cs]} (store/entity-components ~e)]
     ~@expr))

(defmacro entfn 
  [[esymb components] expr]
  `(fn [~esymb] 
     (with-components [~esymb ~components]
       ~expr)))  
(defmacro defproperty [name [esymb components] expr]
  `(defn ~name [~esymb] 
     (with-components [~esymb ~components]
       ~expr)))  

(defn alter-component [e domain f]
  (store/conj-component e 
    (store/->component domain 
      (f  (store/get-component e domain)))))

(defn set-component [e domain v]
  (store/conj-component e 
    (store/->component domain v)))
                        
(defn change-location [e newloc]
  (set-component e :location newloc))
(defn add-to-gold-carried [e amount]
  (alter-component e :gold-carried #(inc % amount)))
(defn increase-fatigue [e]
  (alter-component e :fatigue inc))


(defproperty pockets-full? [e [gold-carried]] (> gold-carried +max-gold+))
(defproperty thirsty? [e [fatigue]]  (> fatigue +max-fatigue+))  

(defstate enter-mine-and-dig-for-nugget 
  {:enter (entfn [ent [name location]]               
                 (if (not=  location :goldmine)
                   (entlog name "Walkin' to the gold mine"
                           (change-location ent :goldmine))
                   ent))
   :execute (entfn [ent [gold-carried fatigue]]
                   (let [updated (-> ent
                                     (add-to-gold-carried)
                                     (increase-fatigue))]
                     (cond (pockets-full? updated)
                           ;;;
                           (thirsty? updated)
                       
                       
              
                   
  

(defentity basic-entity "Every entity in WestWorld has a name and a state."
  [id name init-location init-state] 
  {:components [:location init-location 
                :state init-state]})

(defentity miner "A simple miner, toiling in the earth to earn his keep."
  [id & {:keys [name money-in-bank gold-carried name init-location init-state] :or 
         {name "Bob" money-in-bank 0 gold-carried 0}}]
  {:specs [(basic-entity id name init-location init-state)]
   :components [:money-in-bank money-in-bank
                :gold-carried gold-carried
                :fatigue 0
                :thirst  0]})
