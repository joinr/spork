;;Star Queues: A special extension of priority queues
;;========================================================
;;This type arose from A-star search and other algorithms that have a separate 
;;notion of priority. We maintain a separate mapping of priorities->entries, 
;;and a mapping of nodes->weights.
;;When we conjoin nodes, we have an added hueristic that's applied.  
;;The priority is then hueristic + weight. 
;;If, during the course of a relaxation, we find a shorter path, then we 
;;need to re-weigh the node.  That means finding the priority associated with 
;;the node.  So we maintain two structures: a priority->node map, which is the 
;;typical priority queue of priority->node.  We also maintain a node->priority 
;;map, which is created upon conjing a new node to the priority queue, and 
;;altered when we alter the weight of the node.  Since the priority is a 
;;function of the weight and the heuristic, we need to update the priority 
;;with new information.  If we alter the node weight, we need to supply a 
;;new priority. 

(ns spork.data.starq
  (:require [spork.data.priorityq :refer :all]))

(defrecord star-queue [base-pq nodes])

