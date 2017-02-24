;Core functions and data structures for defining "scenes"
(ns spork.cljgui.scene.core)

;A scene is a collection of at least 3 elements:
;-a hierarchy 
;-a spatial-index 
;-a scene-index 

;Given the above information, we can perform some scene-level operations: 
;-rendering 
;-adding resources 
;-moving resources 
;-dropping resources 
;-merging scenes....

;Ideally, we have a resource map (possibly a simple entity store) that allows
;us to coordinate these seemingly disparate resources, to easily define 
;elements of a scene, compose them into logical hierarchies, and to place 
;them spatially. 

;The scene should be entirely data driven....it's just a store.
;Whatever operates on the scene will pick apart information from the data 
;in the scene, 

;So....we need to be able to draw the scene onto a graphics context, 
;or at least convert the scene into a set of drawing instructions. 

;Let's work with a trivial scene...
;A group containing a white rectangle and a horizontal black line....
;  with a red circle rooted at the end of the black line...

;(def test-scene (->group [(->rectangle :white 0 0 400 400)
;                          (->group [(->line :black 100 100 200 100)
;                                    (->translation 200 100
;                                      (->circle :red 50 50))])]))

;First thing: we'd like to index the components of the scene....
;only we didn't provide and names or other indexing information in the 
;specification.

;what if we had such information?  
;maybe we could use a map to specify the indices...
;{:main {:white-background (->rectangle :white 0 0 400 400)
;        :line-and-ball    {:black-line (->line :black 100 100 200 100)
;                           :offset-circle (->translation 200 100
;                                            (->circle :red 50 50))}}}))

;or, in the absence of indices, we can inject our own with a dfs or some other 
;convention...
;{'circle0      (->circle :red 50 50)
; 'line0        (->line :black 100 100 200 100)
; 'rectangle0   (->rectangle :white 0 0 400 400)
; 'translation0 (->translation 200 100)
; 'compound0 {:group0 {:rectangle1 'rectangle0
;                      :group1 {:shape0 'line0
;                               :group2 [:translation0 ['circle0]}}}}


;{'circle0      (->circle :red 50 50)
; 'line0        (->line :black 100 100 200 100)
; 'rectangle0   (->rectangle :white 0 0 400 400)
; 'translation0 (->translation 200 100)
; 'compound0 {:group0 {:rectangle1 'rectangle0
;                      :group1 {:shape0 'line0
;                               :group2 [:translation0 ['circle0]}}}})
;=> ['circle0 'line0 'rectangle0 
;    'compound0 :group0 :rectangle1 :group1 :shape0 :group2] 


;Note the definition of static resources and primitives, so that everything is 
;defined in the environment. 

;One could traverse this map and evaluate every element.
;The keys of the elements must be unique indices, while the values may be
;expressions or elements mapped to earlier bits of the scene. 

;The convention is pretty straightforward:
;Elements with no parent are root scenes.  They are identified as such. 
;scenes with children are named according to a group index.

{:relations  
 :space 
 :index}

;We can decompose a composite into its components fully.
;We then evaluate the components in the context of what we already know...
;If the components are cached, then we can insert an index in their place.

;Need to extend the IScene type to maps..

(defprotocol ISceneStore
  (get-relations [s])  
  (get-elements [s])  
  (assoc-element [s id])
  (dissoc-element [s id])
  (get-space [s id]) 
)
  

;Note ->  I think I've derived a form that can serve as a general purpose 
;data structure for the scene: 

;see attached image! graph_structure.pdf

;our goal is to create a structure....
;previously, we defined scenes inline...
(def scene-description
  (->group 
    (->translate 10 20
       (->rectangle :red 0 0 20 20))
    (->translate 20 30
       (->rectangle :red 0 0 10 10))
    (->translate 50 50
       (->triangle :red 0 0 20 20))
    (->translate 60 70
       (->group 
         (->circle :red 0 0 10 10)
         (->translate 10 5
            (->arrow :red 0 0 10 5))))))

;If we're doing nothing but scene traversal and drawing, specifically 
;immediate mode rendering, then we can just use the description and blast
;it out as a scene.  The original architecture supports that...no problem.
;The problem is, we usually end up implying a bunch of spacial data and 
;other goodies in the description of the scene, specifically, the local 
;transforms present in the logical hierarchy....they actually delineate 
;spacial bounds that can be used for culling, collision detection, etc. 

;so let's introduce the notion that the above, inline series of node 
;constructors is sufficient to produce a tree of nodes that can be inferred 
;to be a "piece" of scene graph, or a primitive graph.

;The problem is, we'd like to explicitly capture the spatial relations encoded
;in the description.  If we capture the space, we can easily push the spatials 
;into a separate structure that's designed for optimized spatial queries....

;This introduces another problem though.....now that we're querying our scene, 
;if only to share data between structures, we need a uniform way of identfying 
;scene elements between specialized tables.  Specifically, we need an indexing 
;scheme that creates unique indices for the elements that matter, the indices 
;of which are associated with spatial information (and other information).

;So....to accomplish the spatial querying facility, we need a way to index the 
;spatial elements of the scene, and to determine their spatial coordinates in
;a global coordinate space.

;We make the following assumptions:
;Every scene is rooted in a (possibly empty) group node.
;Only geometry and groups have spatial data associated with them.  Group nodes 
;maintain a bounding volume that encloses all of the children.  Leaf nodes, or 
;primitive geometry, maintain a bounding volume that is transformed according 
;to the sequence of state changes dictated by the path from root to leaf.

;Any scene can be distilled into 4 sets of information: 
;description - the description (or source) of the scene, as defined above. 
;scene-nodes - A map of indexed scene nodes, where the keys are the node indices, 
;  and the values are a vector of [label spatial-hash adjacencies bounds]
;  where spatial-hash is a coordinate or other index value into a spatial 
;     structure.
;  where adjacencies is a vector of pairs of the form [scene-node state]
;  bounds is a bounding volume for the node (typically and axis aligned bounding
;    box or some other canonical structure).
;spatial-nodes - A map of spatial data, where the keys are spatial hashes, and 
;   the values are likely [bounds spatial-adjacencies]
;      where spatial-adjacencies is a vector or set of nodes hashed to the 
;      spatial coordinate, and bounds is a bounding volume for the spatial 
;      coordinate. 

;From this abstract notion of a scene, we can manage various aspects using 
;special-purpose data structures.  For instance, the scene hierarchy is 
;nicely preserved in the adjacency-list representation of the scene.  
;The encoded spatial information is now made apparent, and provided to a 
;special purpose data structure that can optimize spatial queries for 
;collision detection, "selecting" or picking, and scene culling.
;The first-class data nature of the scene is now also amenable to various 
;traversals and transormations, which allows us to leverage functional 
;programming and persistent structures. 

;Finally, the scene "should" be able to be neatly converted back into the 
;original expression that spawned it, complete with indexing.  

;How do we get from the primitive inline form into the expanded 3-piece 
;data structure?  

;We must have an indexed set of nodes for the scene....at least the spatial
;nodes.  We may index the intermediate nodes as well, and leave them with 
;no spatial coordinates....this may be preferable...perhaps another table
;that indicates which nodes are geometry, and which nodes are transforms.

;Let's index everything for now....

;The first step is to imagine what a fully anotated scene would look like:

(def annotated-scene-description
  [:group0 (->group 
    [:trans0 (->translate 10 20
       [:rect0 (->rectangle :red 0 0 20 20)])]
    [:trans1 (->translate 20 30
       [:rect1 (->rectangle :red 0 0 10 10)])]
    [:trans2 (->translate 50 50
       [:tri0  (->triangle :red 0 0 20 20)])]
    [:trans3 (->translate 60 70
       [:group1 (->group 
         [:circle0 (->circle :red 0 0 10 10)]
         [:trans4 (->translate 10 5
            [:arrow0 (->arrow :red 0 0 10 5)])])])])])

;spatial data for each of the primitives....
;note, that group nodes should contain bounding boxes that enclose their 
;children.
(def annotated-scene-description
  [:group0 #BoundingBox[10 20 80 80] 
   (->group                
    [:trans0 
     (->translate 10 20
       [:rect0 #BoundingBox[10 20 20 20] 
        (->rectangle :red 0 0 20 20)])]
    [:trans1 
     (->translate 20 30
       [:rect1 #BoundingBox[20 30 10 10] 
        (->rectangle :red 0 0 10 10)])]
    [:trans2 
     (->translate 50 50
       [:tri0 #BoundingBox[50 50 20 20]  
        (->triangle :red 0 0 20 20)])]
    [:trans3 
     (->translate 60 70
       [:group1 #BoundingBox[60 70 20 10] 
        (->group 
         [:circle0 #BoundingBox[60 70 10 10] 
          (->circle :red 0 0 10 10)]
         [:trans4 
          (->translate 10 5
            [:arrow0 #BoundingBox[70 75 10 5] 
             (->arrow :red 0 0 10 5)])])])])])
;another transformation...
(def annotated-scene-description
  {:group0 node{:data {:bounds #BoundingBox[10 20 80 80]} :children 
    {:trans0 node{:data #translation{:x 10 :y 20} :children 
        {:rect0 node{:data {:bounds #BoundingBox[10 20 20 20]
                            :geometry (->rectangle :red 0 0 20 20)}}}}}                                         
    {:trans1 node{:data #translation{:x 20 :y 30} :children 
        {:rect1 node{:data {:bounds #BoundingBox[20 30 10 10]
                            :geometry (->rectangle :red 0 0 10 10)}}}}}
    {:trans2 node{:data #translation{:x 50 :y 50} :children 
                  {:tri0 node{:data {:bounds #BoundingBox[50 50 20 20]  
                                     :geometry (->triangle :red 0 0 20 20)}}}}}
    {:trans3 node{:data #translation {:x 60 :y 70} :children 
       {:group1 node{:data {:bounds #BoundingBox[60 70 20 10]} :children  
         {:circle0 node{:data {:bounds #BoundingBox[60 70 10 10] 
                               :geometry (->circle :red 0 0 10 10)}}}
         {:trans4 node{:data #translation{:x 10 :y 5} :children 
                       {:arrow0 node{:data {:bounds #BoundingBox[70 75 10 5] 
                                            :geometry (->arrow :red 0 0 10 5)}}}}}}}}}}})

;another transformation...if we just make nodes vectors....
(def annotated-scene-description
  {:group0 [{:bounds #BoundingBox[10 20 80 80]} 
    :trans0 [#translation{:x 10 :y 20}  
             {:rect0 [{:bounds #BoundingBox[10 20 20 20]
                       :geometry (->rectangle :red 0 0 20 20)}]}]                                         
    :trans1 [#translation{:x 20 :y 30}  
             {:rect1 [{:bounds #BoundingBox[20 30 10 10]
                       :geometry (->rectangle :red 0 0 10 10)}]}]
    :trans2 [#translation{:x 50 :y 50}  
             {:tri0 [{:bounds #BoundingBox[50 50 20 20]  
                      :geometry (->triangle :red 0 0 20 20)}]}]
    :trans3 [#translation {:x 60 :y 70} 
             {:group1 [{:bounds #BoundingBox[60 70 20 10]}  
                       {:circle0 [{:bounds #BoundingBox[60 70 10 10] 
                                   :geometry (->circle :red 0 0 10 10)}]
                        :trans4 [#translation{:x 10 :y 5}  
                                 {:arrow0 [{:bounds #BoundingBox[70 75 10 5] 
                                            :geometry (->arrow :red 0 0 10 5)}]}]}]}]]})

;so....the form is this: 
;maps are inferred to be group nodes. 
;rather than a special node data type, we just use a vector pair,[data children]
;where data is a special node-type, and children is a map of child names to 
;child nodes. Note: added a new node-type, for objects that support shape, 
;we have a geometry node.
;The only nodes that have bounds are geometry and group.  
;Group nodes 
(def annotated-scene-description
  {:group0 [{:bounds #BoundingBox[10 20 80 80]} 
    :trans0 [#translation{:x 10 :y 20}  
             {:rect0 [{:bounds #BoundingBox[10 20 20 20]
                       :geometry (->rectangle :red 0 0 20 20)}]}]                                         
    :trans1 [#translation{:x 20 :y 30}  
             {:rect1 [{:bounds #BoundingBox[20 30 10 10]
                       :geometry (->rectangle :red 0 0 10 10)}]}]
    :trans2 [#translation{:x 50 :y 50}  
             {:tri0 [{:bounds #BoundingBox[50 50 20 20]  
                      :geometry (->triangle :red 0 0 20 20)}]}]
    :trans3 [#translation {:x 60 :y 70} 
             {:group1 [{:bounds #BoundingBox[60 70 20 10]}  
                       {:circle0 [{:bounds #BoundingBox[60 70 10 10] 
                                   :geometry (->circle :red 0 0 10 10)}]
                        :trans4 [#translation{:x 10 :y 5}  
                                 {:arrow0 [{:bounds #BoundingBox[70 75 10 5] 
                                            :geometry (->arrow :red 0 0 10 5)}]}]}]}]]})


;some ways to get to an annotated scene description...
(def scene-description
  {:group0 (->group {:trans0 (->translate 10 20
                       {:rect0 (->rectangle :red 0 0 20 20)})
                     :trans1 (->translate 20 30
                       {:rect1 (->rectangle :red 0 0 10 10)})
                    :trans2 (->translate 50 50
                       {:tri0 (->triangle :red 0 0 20 20)})
                    :trans3 (->translate 60 70
                       {:group1 (->group
                            {:circle0 (->circle :red 0 0 10 10)
                             :trans4  (->translate 10 5
                                       {:arrow0 (->arrow :red 0 0 10 5)})})})})})

;if we establish the convention that maps are groups....then we can drop the
;(->group ...)  calls.
(def scene-description
  {:group0 {:trans0 (->translate 10 20 {:rect0 (->rectangle :red 0 0 20 20)})
            :trans1 (->translate 20 30 {:rect1 (->rectangle :red 0 0 10 10)})
            :trans2 (->translate 50 50 {:tri0  (->triangle :red 0 0 20 20)})
            :trans3 (->translate 60 70
               {:group1 {:circle0 (->circle :red 0 0 10 10)
                         :trans4  (->translate 10 5
                                     {:arrow0 (->arrow :red 0 0 10 5)})}})}})

;even better...
(def scene-description
  (let [rect0 (->rectangle :red 0 0 20 20)
        rect1 (->rectangle :red 0 0 10 10)
        tri0  (->triangle :red 0 0 20 20)
        circle0 (->circle :red 0 0 10 10)
        arrow0  (->arrow :red 0 0 10 5)]        
   {:group0 {:trans0 (->translate 10 20 {:rect0 rect0})
             :trans1 (->translate 20 30 {:rect1 rect1})
             :trans2 (->translate 50 50 {:tri0 tri0})
             :trans3 (->translate 60 70 
                {:group1 {:circle0 circle0 
                          :trans4  (->translate 10 5
                                      {:arrow0 arrow0})}})}}))

;in the case of the original scene description, we can convert it into the 
;annotated scene by traversing the nodes, and replacing said nodes with 
;generated indices. 

;first step is to convert from the node representation into the annotated 
;scene form, then add bounds information. 
;Once bounds information is complete, we have a bounds map: 
;scene-node -> bounds.
;Extract the bounds map, computing spatial hashes for the bounds.

;Assoc the [node-index bounds] onto the spatial at spatial-hash.
;Assoc the [node-index spatial-index] onto the spatial-map.

;In parallel....
;Compute the node-list for each node. 
;which is a map of {node-index [node-label bounds [adjacencies]]}

;so we need some operations....

(defprotocol ISceneIndexed 
  (assoc-description [s d] "Associate the scene with a description.") 
  (assoc-relation [s idx label spatial adj] "Associate a node onto the scene, at index.")
  (dissoc-scene [s idx] "Drop a node, and its children, from the scene.") 
  (get-scene [s idx] "Get the node ")

  (assoc-spatial [s idx space])
  (dissoc-spatial [s idx])
  (get-spatial [s idx]) 
  (get-spatial-hash [s space]))
;let's get it working with a map first...

(defn (->empty-scene {:description nil
                      :node-map {} 
                      :spatial-map {}
                      :spatial {}}))


(def scene-description
  (->group 
    (->translate 10 20
       (->rectangle :red 0 0 20 20))
    (->translate 20 30
       (->rectangle :red 0 0 10 10))
    (->translate 50 50
       (->triangle :red 0 0 20 20))
    (->translate 60 70
       (->group 
         (->circle :red 0 0 10 10)
         (->translate 10 5
            (->arrow :red 0 0 10 5))))))

(def scene-description
  (let [rect0 (->rectangle :red 0 0 20 20)
        rect1 (->rectangle :red 0 0 10 10)
        tri0  (->triangle :red 0 0 20 20)
        circle0 (->circle :red 0 0 10 10)
        arrow0  (->arrow :red 0 0 10 5)]        
   {:group0 {:trans0 (->translate 10 20 {:rect0 rect0})
             :trans1 (->translate 20 30 {:rect1 rect1})
             :trans2 (->translate 50 50 {:tri0 tri0})
             :trans3 (->translate 60 70 
                {:group1 {:circle0 circle0 
                          :trans4  (->translate 10 5
                                      {:arrow0 arrow0})}})}}))


;in an annotated scene, we can find a path from any child to the root 
;by following parent or predecessor nodes...

;Say we want to modify a piece of the scene, by updating the transform for 
;the arrow in the circle/arrow structure of group1....

;Let's say the user somehow hits the arrow, i.e. selects it with the mouse.
;the hit would register at the arrow...which is a leaf node...
;what if we wanted to modify it's position, so that it's transformed 
;5 units to the left? 

;We need to update the transform associated with the shape...
;in this case, we have a parent transform....:trans4

;Say we somehow decided to change the arrow by shifting it 5 units to the left.
;Currently, we just replace the red arrow with a new arrow...
;The new arrow is (->arrow :red 5 0 10 5), reflecting a change in space....
;If we want to edit the new arrow, we need to compute its bounds....
;this means traversing up the to the root node ( a node with no parent) 
;since we have an adjacency list, and each node can only have one parent, 
;we can easily do this using our parent indices...

;starting with :arrow0, while parent is not nil, push the node onto the path.
;set the parent = new node, recur. 
;Once the root has been reached, we have accumulated a path (and if we're smart,
;we've transformed the local bounds from our arrow along the way)....

;At this point, we need to perform a final update, which is to add the world 
;bounds of the arrow...

;There's a higher order function here.....propogate...
;We're propogating information from a leaf node, up through the parents, 
;and finalizing with an edit to the leaf node (maybe).

;A few smart optimizations...
;We should bring along the node-set and the spatial-set, as we build the path 
;from the leaf to the root.  Since our leaf node is already indexed in both 
;of these sets, we can update its state as we run across transforms in our 
;paths...

;I think propogate-from-leaf looks like this...
(defn reduce-from-leaf
  "Given a scene of {:scene-nodes ... :spatial-nodes ...}, and a valid index 
   to an entry in scene-nodes, we perform a reduction step that walks the node
   back from the leaf up to the root of the scene. Our accumulator is the 
   [scenemap leafindex [path]], and the values we reduce are the parents of 
   the parents of the leafidx.  We reduce using the f, which accumulates 
   a result up from the leaf to the root.  After the propogation reduction has
   finished, we apply finalf to the accumulated result.  This is envisioned as 
   a means to propogate changes - likely spatial transforms - from leaf nodes
   to parent nodes, throug the root of the tree, to ensure that spatial bounds
   are up to date, and that the spatial nodes are updated as necessary."
  [scene leafnode f root-func]
  (reduce (fn [state current-node]
            (f state current-node))
          {:scene scene 
           :from-node leafnode}  )

(defn reduce-to-leaves
  "Like propogate-from-leaf, except we apply a downward-pushing function that 
   accumulates [scenemap leafindex [path]] for every child of the children of 
   startnode, "  
  [scene startnode f leaf-func]
  )

(defn predecessors 
  "Fetches a sequence of nodes, via the parent relationship, from the startnode
   to root."
  [scene startnode])

;(defn successors
;  "Fetches a forest of node sequences, in depth-first order,  "
;  [scene startnode])

;we can save parent-child information outside of the nodes...
;all that's important is that we index the nodes...

(def annotated-scene-description
  {:group0 [{:bounds #BoundingBox[10 20 80 80]} 
    :trans0 [#translation{:x 10 
                          :y 20}  
             {:rect0 [{:bounds #BoundingBox[10 20 20 20]
                       :geometry (->rectangle :red 0 0 20 20)}]}]                                         
    :trans1 [#translation{:x 20 
                          :y 30}  
             {:rect1 [{:bounds #BoundingBox[20 30 10 10]
                       :geometry (->rectangle :red 0 0 10 10)}]}]
    :trans2 [#translation{:x 50 
                          :y 50}  
             {:tri0 [{:bounds #BoundingBox[50 50 20 20]  
                      :geometry (->triangle :red 0 0 20 20)}]}]
    :trans3 [#translation {:x 60 
                           :y 70} 
             {:group1 [{:bounds #BoundingBox[60 70 20 10]}  
                       {:circle0 [{:bounds #BoundingBox[60 70 10 10] 
                                   :geometry (->circle :red 0 0 10 10)}]
                        :trans4 [#translation{:x 10 
                                              :y 5}  
                                 {:arrow0 [{:bounds #BoundingBox[70 75 10 5] 
                                            :geometry (->arrow :red 0 0 10 5)}]}]}]}]]})

(defn annotate-node [((->group {}) :as node) ({} :as env)]
  ;compute a label for the node...
  (let [lbl (compute-label env node)
        bounds (get-bounds node)  
  (assoc env  
   (->node (assoc (node-data node)
                  {})))
 
(defn ->node-entry [label & {:keys [ data parent space-hash adjacency]}]
  [label {:data data 
          :parent parent
          :space-hash space-hash 
          :adjacency adjacency}])

{:group0 
 node{:data #group{:bounds :somebox} 
      :children 
      {:trans0 
       node{:data #translation{:x 10 :y 20} 
            :children   
            {:rect0 
             node{:data #shape{:bounds #BoundingBox[10 20 20 20]
                               :geometry (->rectangle :red 0 0 20 20)}
                  :children nil}}}}}}


(defn node? [x]  (every #{:data :children} x))

;Note -> if we're going to be doing lots of path lookups....we can do the 
;search once, and cache it....for instance, the hierarchical information for
;the paths is unlikely to change during the course of anything...however, 
;we might need to constantly update a node due to transformations and other 
;things, every frame.  One way around this is to use cached paths...
;We can keep cached paths in the structure somewhere as an optimization...

(description->indexed-scene
  "Given a scene and a labeling function, will convert the scene into an 
   indexed scene, as well as extract parent-child relations from the scene.
   This is more or less a first step in the pipeline to building a spacially 
   indexed scene.  There's a catch....we allow users to splice pre-indexed 
   elements into a scene as maps.  So we have to cover the case where the 
   scene node is a map.  In that case, it's technically an indexed group."
  [scn & {:keys [make-label] :or {make-label (fn [m c] (gensym))}}]
  (let [labeled-node (fn [x] (cond (map? x) {:label  (first (keys x))
                                             :node (first (vals x))}
                                   ))]
    (loop [node-order [] ;necessary?  
           labels #{} 
           node-map {}
           fringe [(to-fringe scn)]]    
    (if (empty? fringe) {:node-order node-order 
                         :node-map node-map}      
      (let [{:keys [parent label node]} (first fringe)     
            [new-labels children]  
            (reduce (fn [[ls xs] child]
                      (let [lbl (make-label ls child)]
                         [(conj ls lbl)
                          (conj xs {:parent parent
                                    :label lbl 
                                    :node child})]))
                    [(conj labels node-label) []] 
                    (get-children node-data))]
        (recur (conj node-order node-label)
               new-labels 
               (assoc node-map (->node-entry 
                                 node-label 
                                 :data node-data 
                                 :parent node-label
                                 :space-hash nil ;fill this out...
                                 :children (vec (map first children))))
               (into (subvec fringe 1) children)))))
               
                                              
                                               


              

  
      
    
  
                      
                      






