;A clojure implementation of Conrad Barksi's Orc Battle.
(ns spork.entitysystem.examples.orcbattleLOL)


(declare init-monsters init-player gameloop)
(defn fresh-line "added for cl compat" [] (println \newline))
(defn rand-val [n] (inc (rand-int n)))

(defrecord playerstats [health agility strength])

(defn make-player []  (->playerstats 30 30 30))

(defrecord gamestate [player monsters monster-num])
(defn make-gamestate [] (->gamestate (make-player) nil nil 12))

(defn game-msg 
  ([g pred msg] (when (pred g) (println msg)) g)
  ([g msg] (game-msg g (fn [_] true) msg )))

(defn player-dead? [g] (<= (-> g :player :health) 0))
(defn monsters-dead? [g] (= (-> g :monster-num) 0))

(defn terminate-game [g] 
  (-> g
    (game-msg player-dead? "You have been killed. Game Over." )
    (game-msg monsters-dead? 
       "Congratulations! You have vanquished all of your foes.")))

(defn orc-battle [gamestate monster-builders]
  (-> gamestate 
     (init-monsters monsterbuilders)
     (init-player)
     (gameloop monster-builders)
     (terminate-game)))

(defn player-moves
  "Conrad inlines this, but I separated it for testing."
  [player] (inc (int (/ (max 0 (-> player :agility)) 15))))

(defn game-loop [g ]
  (while (not (or (player-dead? g) (monsters-dead? g)))
    (show-player g)
    (loop [state g
           attacks (player-moves (:player g))]
      (if (or (<= attacks 0) (monsters-dead? state))
        state
        (do (show-monsters (:monsters state))
            (recur (player-attack state) (dec k)))))))

(defn show-monsters [coll] (doseq [m coll] (show-monster m)))
(defn show-player
  "Use map destructuring to get our args easily."
  [{:keys [health agility strength] :as player}] 
  (str "You are a vigilant knight with a health of "  health
       ", an agility of "  agility
       ", and a strength of " strength))

(defn attack-damage [attack strength]
  (case attack 
    :stab (+ 2 (rand-val (Math/abs (dec strength))))
    :double-swing (rand-val (int (/ strength 6)))
    (rand-val (int (/ strength 3)))))

(defn attack-description [attack damage]
  (str "Your " (str attack) " has a strength of " damage))

(defn random-key 
  ([m ks]  (nth ks (rand-int (count ks))))
  ([m] (random-key m (into [] (keys m)))))
        
(defn random-monster [g] (random-key (:monsters g)))
(defn pick-monster [g] 
  (do (fresh-line)
      (print-ln "Monster #:")
      (let [x (get-line)]
        (if (not (int?


;(defn apply-player-attack [g attack monsters] 
;  (let [p (:player g)]
;    (case attack
;      :stab (+ 2 (randval  
        
        
      
      