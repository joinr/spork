;from barski's book....tailored for an entity system using component based 
;architecture....

(ns spork.entitysystem.examples.orcbattle 
  (:use [entitysystem.store]))

;A record to hold the gamestate....we only really 
;need to keep track of our entities (via an entity store) 
;and the cap on the total number of monsters.
(defrecord gamestate [entities monsternum])
(def new-game (->gamestate emptystore 12)) 

;Component definitions....these are building blocks for domains of interest 
;in our game (or simulation...depending on your point of view).....
(defcomponent coords
  "A simple set of 2D coordinates."
  [xy] xy)
(defcomponent basicstats 
  "Stats that all active entities share."
  [{:keys [health agility strength] :as stats}] 
  stats)
(defcomponent combatstats
  "Entities that are capable of offense."
  [{:keys [offense defense] :as stats}] 
  {:offense offense 
   :defense defense})
(defcomponent race 
  "Entities that have a race."
  [race] race)
(defcomponent timer
  "A timer with an initial time."
  [init] init)
(defcomponent deathcry
  "Entities that evoke a cry when they perish."
  [description] description)
(defcomponent visage
  "A text description of the entity."
  [description] description) 

;(defcomponent events [] (initial-schedule))
;(defcomponent events [] )


;Utility functions for things like entity stats and 
;other minor stuff...
(def nonzero-int (comp inc rand-int))
(defn rand-int-between
  "Returns a random integer between x1 and x2.."
  [x1 x2]  
  (+ (rand-int (- (inc x2) x1)) x1))  

(defn wimpy-stat [] (rand-int-between 1 10))
(defn strong-stat [] (rand-int-between 10 20))
(defn super-stat [] (rand-int-between 20 30))

(defn random-stats
  "Rolls up a set of random statistics for health, agility, and strength.
   Stats can be overriden using key arguments."
  [& {:keys [health agility strength]}]
    {:health (default health (wimpy-stat)) 
     :agility (default agility (wimpy-stat))
     :strength (default strength (wimpy-stat))})

(defn brawler-stats []
  (random-stats :health   (strong-stat) 
                :strength (super-stat)))

(defn rogue-stats []
  (random-stats :health   (wimpy-stat) 
                :strength (wimpy-stat)
                :agility  (super-stat)))

(defn boss-stats []
  (random-stats :health (super-stat)
                :strength (strong-stat)
                :agility (strong-stat)))

;Entity definitions for the player and a stable of monster archetypes.

;We will use these as templates and vary them accordingly.
;Rather than defining classes and using OOP style inheritance, encapsulation, 
;blah....the defentity macro simply provides a convenient way for building 
;functions that package components together in a high-level fashion.  At the 
;end of the day, all of the "entities" we define are really just a collection 
;of the components defined above, with the ability to extend the definitions 
;trivially via inline component definitions, inline or anonymous entity specs, 
;and deriving from previous entity definitions.  
(defentity prop [id & {:keys [description coordinates]
                       :or {description "It defies description"
                            coordinates {:x 0 :y 0}}}]
  "The simplest set of components that define entities that have a position."
  [coords coordinates
   visage description])
    
(defentity combatant [id & {:keys [offense defense] 
                            :or {offense 10 defense 0}}]
  "A basic set of components indicating an entity's ability to engage
   in combat, and to what extent."
  {:components 
   [combatstats {:offense offense :defense defense}
    :offensive (> offense 0)
    :defensive (> defense 0)]})   

(defentity player [id]
  "A simple template for human players.  The initial game only anticipates one
   human player, or one player-controlled entity, but the advent of components
   means that we may extend control to multiple entities, or allow the player 
   to take control of enemies (perhaps with a skill)..."
  {:specs [combatant
           (prop id :description 
                 (str "The remnant of a lost age, standing alone against the evil that"
                      " plagues this land..."))]
   :components 
    [basicstats  {:health 30   :agility 30 :strength 30}
     :playertag (keyword (str "player" id))]})

(defentity monster [id & {:keys [name race stats vis]}]
  "A generic monster, with possibly random stats.  Useful for prototyping, and
   provides a base set of components that may be easily over-ridden in other 
   entity definitions."
  {:components
   [basicstats (default stats (random-stats))
    :race      (default race :generic)
    :monster   true
    visage     (default vis 
                 (str "The " name " defies description!"))
    :enemy     true]})

(defn simple-monster [race & [stats &rest]]
  (monster nil :race race
               :stats stats))
(defentity orc
  "Orcs are simple monsters...vicious in nature.
   Creates a random orc."
  [id & {:keys [orcstats] :or {orcstats (brawler-stats)}}]
  [(simple-monster :orc orcstats)
   combatant]
  [:damage-modifier (inc (rand-int 8))  
   visage "A wicked orc!"])

(defentity rogue
  "Rogues are agile enemies with weak attacks.  They have 
   the ability to snare opponents in a net, paralyzing them 
   for a round.  Creates a random rogue."
  [id & {:keys [net-probability roguestats] 
         :or   {net-probability 0.1 
                roguestats (rogue-stats)}}]
  [(simple-monster :rogue roguestats)
   combatant]
  [:effects {:paralyze net-probability}
   visage "An evil rogue!"])
  
(defentity hydra
  "Hydras are multi-headed beasts that can regenerate health.
   Creates a random hydra."
  [id] 
  [(simple-monster :hydra)
   combatant]
  [visage "A Malicous hydra!"
   :effects {:regeneration 1}])

(defentity slime-mold
  "Slime-molds are weak monsters that slow down prey, feasting 
   after starvation or suffocation takes hold.
   Creates a random slime-mold."
  [id]
  {:specs  [(simple-monster :slime-mold)
            combatant]
   :components  [visage "A slime mold!"
                 :effects {:drain :agility}]})

(defentity testent
  "A test entity"
  [id]
  {:specs [(simple-monster :test)
           combatant]})
 
(defentity slime-rogue [id] 
  "A slime molde with roguish aspects" 
  {:specs [rogue slime-mold (monster id :race :slime-rogue 
                                        :vis "A roguish slime-mold!")]})
(defentity slime-orc [id] 
  "An orc with paralyzing capabilities..."
  {:specs [slime-mold 
           orc]})
   


