(ns spork.cljgui.scene.maptree)


;it might be nice to have a with-indexing macro...
;the idea is that, when we build nested structures, with-indexing will 
;create a unique index for us that maps the nested structure into a 
;dependency graph. 

;so we can work with normal nested structures, like [1 2 3 [3 4]]
;have with-indexing take care of traversing the structure to build 
;indices....

;another idea is to have a set of paths...
;i.e., we can get to any node from root....
;so root knows all paths (like an SPT tree). 


(comment 
;we can represent a tree in an array thusly:

;we currently have a node-based structure that encodes a tree...
;the-tree: 
{:data {:name :root}
 :children [{:data {:name :child1} :children nil}
            {:data {:name :child2} :children nil}
            {:data {:name :group1} 
             :children [{:data {:name :child3}}]}]}

;this defines a multi-way tree....
         [root]
       /   |   \ 
[child1 child2 group1]
                  |
                [child-1]
                
;an equilavent, map-based structure is: 
                
{:nodes {:root {:name :root}
         :child1 {:name :child1}
         :child2 {:name :child2}
         :group1 {:name :group1}
         :child3 {:name :child3}}
 :children  {:root   #{:child1 :child2 :group1}  ;outbound arcs
             :group1 #{:child1}}
 :parents {:child1 #{:root}                      ;inbound arcs...
           :child2 #{:root}
           :group1 #{:root}
           :child3 #{:group1}}}

;I'm calling this a map-backed tree. 
;Note, it can be seen as a graph (and will likely have conversion operations
;associated with it), but I'm not starting from the view that it's a generic
;graph...

;we can define operations to build this map...
;we need a function that can extract an index from each node...
;  in the above example, it's just the :name function. 

;what does an empty mbt looke like? 
{:nodes nil 
 :children nil
 :parents nil}

;what happens when we add a node? 
;we need a way to generate an index from the node....
;or we provide the index to the append? 
)

;Me must assume that the data is indexable with the tree...
;And that children can be extracted from the data...
;We're really associating a node, as defined in cljgui.scene....
;This should probably be were the node interface lives. 
(defprotocol INodeMap 
  (get-index [n m])
  (get-children [n])
  (get-data [n]))  

(defn add-node [mbt data] 
  (assoc-in mbt [:nodes (get-index data mbt)] data))

;(defn append-child [mbt idx data]
  ;the consequence here is a bit moer
  
          
         
                