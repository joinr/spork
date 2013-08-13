(ns spork.cljgui.scene.manager
  (:use [cljgui.scene.scenegraph]))


;Note -> this file is still largely experimental.  I'm trying to work this
;out.

;we track - what is currently a "traditional" scene, 
;as one or more relations.  spatial data, in the form of a quadtree, 
;maintains information for culling and collision detection. 
;entities are all of the primitive leaf nodes (specifically IShapes or other
;geometry, transformed into world-coordinates.
;This is basically a database that coordinates the logical/semantic relations
;between elements, and the spatial relations.

;We can define scenes declaratively, as before, but we need to conjoin them 
;into a higher-order scene-management system. 
;Note -> this bears a striking resemblance to our component entity system...
;and I'll probably adapt it later.
(defrecord scene-store [relations spatial entities])

;We define scenes by using the existing declarative forms and functions...
;i.e. defining transforms and state changes, which equate to rendering paths.

;Any scene, once constructed, can be transformed into a sequence of leaf nodes.
;In the case of a scene context, our leaf nodes correspond to entities in the
;scene...specifically primitive shapes (in a 2D context).  In any scene, there
;are guaranteed to be a finite amount of unique entities (or entity values). 
;In practice, this conform to a set amount of primitive shapes or meshes, or
;other resources, which are loaded once and serve as shared resources for 
;multiple "entities".  In clojure, with its referential transparency and 
;focus on immutable values, we get this for free...

;As such, we can process the relation defined by the scene, for a set of 
;unique "values" for the leaves.  These leaves form the set of all entities in
;the scene, and must be uniquely (and ideally permanently) identified, since 
;the entities may be dropped from the scene.  Further, the entities are mapped
;to a spatial component which facilitates specialized operations like collision
;detection,range queries, and culling.

;So....we currently define scenes without any naming or indexing...
;We either need to enforce a name, or gensym a name automatically, to ensure
;we have unique names. 

;Assuming the current scene structure, we can transform the relations (the 
;traditional "scene-graph" structure, into a flat sequence of shapes, 
;transformed to absolute (or world) coordinates.  Each entity can be assigned
;a unique ID.  

;Once transformed, the entity bounds can be computed (they already exist).
;The spatial bounding information is then stored in a quadtree, or other 
;spatial data structure.  We essentially drop a bunch of then entities into 
;the quadtree, affecting a spatial partitioning.  

;As we assoc new scenes to the existing scene-store, we pipe them through the
;same process: 
;evaluate the scene relation, associating unique names to leaves (in metadata).
;add each entity to the quadtree

;This is a good overall framework....

;We should now have a few extra operations that we can support: 
;Render the scene-store
;Traverse the scene-store
;Select entities in the scene-store 
;Perform operations on the scene-store, or certain entities within, namely: 
;  translate, rotate, etc. 
;Associate new scenes into the scene-store. 
;Query the scene store for intersections/collisions based on bounds. 

;So....a scene-store is just like any other scene, except it has special 
;properties (and overhead). 
;Namely, the scene-store caches spatial information, creates unique indices 
;for primtives in the scenes, and allows for naming of hierarchical relations.

;The scene store also allows us to propogate information throughout adjacent
;elements, namely enforcing parent/child relations. The standard imperative 
;notion of "moving" a leaf node by modifying its transform is, in an indexed
;scene, easily handled by propogating any changes up through parent nodes.

;Another option is, instead of indexing the scene graph explicitly, we 
;define paths to children, from the root, as indices.  This way, every 
;shape has a path associated with it, from its parent...
;When we cache the scene in a spatial index, we cache the indexed path along
;with the bounding volume.  So when we get a mapping back into the original 
;scene, which is a path to the child node starting from the scene root. 

;Merging scenes means merging root information....this is actually quite 
;easy, since each scene has a root...which has paths to all children...
;When we merge scenes, we're either subordinating one to the other, or 
;making them siblings...subordinate to a new root.  
;In the sibling case, this constitutes 

;Note -> we can further modify the relations by additional specialization: 
;Transforms, and rendering primitives. 

;So, for each entity, we have a series of transforms that must hold. 

;And, for each entity, we have an accumulated rendering state that describes
;the rendering context.

;We know where each entity is in space, and can quickly test collision against
;other entities. 

;Also, the scene-bounds of the scene store are, implicitly, the quadtree. 

(def lineball 
  (->translation (halve 500) (halve 500)
   (->group 
     [(->circle :red -20 -20 20 20)
        (->translation 20 20
        (->line :black -20 -20 20 20))])))

;Another option....we can imply that anything that isn't named, is 
;automatically aggregated. 
;One way of looking at this is to "compile" a scene.  Traverse the scene 
;graph, turning the transforms in the relation into absolute, primitive 
;shapes. 

(defscene lineball [] 
  {:lineball {:line-and-ball
              (->group 
                [:red-ball (->circle :red -20 -20 20 20)
                 :black-line (->translation 20 20
                                (->line :black -20 -20 20 20))])}})











