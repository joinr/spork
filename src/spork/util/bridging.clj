;Tom Spoon 4 July 2012.
;This is a simple library for solving a common problem: bridging data from 
;external sources during import.  In my case, the catalyst was having a team
;member submit data that "sort of" conformed to a shared specification, but 
;not directly.  Specifically, the fields needed for the specification existed, 
;but the names were altered, order was different, and some required fields were
;missing.  I resolved to build a little library to facilitate building quick 
;specifications for data bridges, basically maps that facilitate the re-mapping
;of source data to a new specification. This could be thought of as a special 
;case for SELECT queries in SQL, but a bit more composable. 
(ns spork.util.bridging
  (:require [clojure.java [io :as io]]
            [clojure [set :as setlib]]))

;(defn ensure-set [s]
;  (do (assert (sequential? s) (str s " is not compatible with hash-set"))
;      (if (set? s) s 
;          (into #{} s))))

;Our library of registered bridge maps.  This allows external libs to use 
;defbridge to add new bridge vars, and to store their schema here.  Keeps the 
;data accessible.
(def bridges (atom {})) 

;Philosophically, the point of a bridge is to cross a chasm....in this case, 
;the chasm between some source data and a desired format; we want to rectify
;the problems in the source data to make it palatable (for some other use).

;bridges are composed of elements that relate fields in source data to a new
;field, along with a value to use if the source field does not exist.  At their
;narrowest, bridges simply map only the fields in some source data.  However, 
;as is often the case, we want to also expand the data to add new fields, 
;usually with some default or missing value.  We can accomplish this using 
;SELECT queries to remap the field names, and to add the missing fields.  

;To make it simpler, bridges that define relations for unmatched fields in the 
;source data will add the new fields as necessary, using a default value.
;Fields not addressed by the bridge are dropped by default.

(defn bridge-field
  "Defines valid ways to bridge a given sourcefield using an assumably foreign 
   field, target.  Optional not-found value serves as the 
   default, in cases where no sourcefield cannot be bridged."
  ([sourcefield target not-found]  [sourcefield [target not-found]])
  ([sourcefield target] (bridge-field sourcefield target nil))
  ([sourcefield] (bridge-field sourcefield sourcefield nil)))

(defn get-target
  "Returns the target field associated with bridgefield bfield"
  [bfield]  (fnext bfield))
(defn get-source
  "Returns the source field associated with bridgefield bfield"  
  [bfield] (first bfield))
(defn get-default
  "Returns the default value associated with bridgefield bfield"
  [bfield] (fnext (get-target bfield)))
(defn set-default
  "Returns the result of changing the default value for bridgefield bfield.
   Useful for extending a bridge template, for instance..."
  [bfield v] 
  [(get-source bfield) [(first (get-target bfield)) v]])

(defn set-defaults
  "Returns a sequence of bridge fields that have v as a default value, with all
   other attributes deriving from the bridge fields in bfs."
  [v bfs] 
  (map #(set-default % v) bfs))

(defn field-ordered-map
  "Ensures the ordering of keys is maintained."
  [kvps]
  (let [fieldorder (reduce (fn [acc [k & rest]] 
                             (assoc acc k (count acc))) {} kvps)
        orderf  (fn [k] (get fieldorder k Double/POSITIVE_INFINITY))]
    (sorted-map-by
      (fn [k1 k2] (compare (orderf k1) (orderf k2))))))

(defn make-bridge
  "Given a sequence binds:: [[k v & [default]] ...], returns a bridge map, 
   an associative structure where keys are source fields that can be bridged, 
   and vals represent the context for bridging, a [targetfield defaultval] 
   pair."
  [binds]
  (let [bridge 
        (for [b binds] 
          (apply bridge-field b))]                                                 
    (into (field-ordered-map bridge) 
          (map vector (map get-source bridge) (map get-target bridge)))))



;library functions for registering bridges....
(defn add-bridge
  "Adds bridge b to the bridge library contained in bridges."
  [name b] (swap! bridges assoc name b))
(defn get-bridge
  "Retrieves the bridge assocated with name from bridges."
  [name] (get @bridges name))
(defn list-bridges
  "Returns a hashmap of all registered data names, associated with their 
   corresponding bridge maps."
  [] @bridges)

(defn bridge->template
  "Converts a bridge map into a flattened template represenation, where a 
   template is a sequence of triples, [[sourcefield targetfield defaultval] ...]"
  [bridge] 
  (into [] (for [[k [v d]] bridge] [k v d])))

(defn template->bridge
  "Alias for make-bridge, to be applied to a sequence t, where 
   t is composed of [[sourcefield targetfield & [defaultval]]...] triples."
  [t] (make-bridge t))


(def NA "N/A") 

(defmacro defbridge
  "Macro to manage the definition of bridge maps.  defbridge consumes a name a
   a template - a sequence of [sourcefield targetfield defaultvalue] triples, 
   computing a bridgemap, registering the brigemap in bridges under
   name keyword, and binds a var to the bridgemap."
  [name template]
  `(let [br# (make-bridge ~template)]
     (do (def ~name br#)
         (add-bridge ~(keyword name) br#))))

(defn translate-map
  "Given a bridging specified by bridgemap, and a source map m, returns the 
   result of remapping m's keys to those specified in the bridgemap.  The 
   resulting map's keys include only the bridge keys.  For fields not bridged 
   during the translation, default values are drawn from the bridgemap."
  ([bridgemap fieldmap m]  
	  (let [fields (into #{} (keys m)) ;possible source keys
	        bridges (into #{} (keys bridgemap)) ;all keys we're going to produce
	        mapped-fields (setlib/intersection fields bridges) ;keys we can directly produce
	        ;if k exists in both fields and bridges, return [(first (get bridgemap k)) v]
	        ;if k does not exist in both fields and bridges...
	        ;  return (get bridgemap k)
	        static-fields (setlib/difference bridges mapped-fields) ;keys we have to remap using defaults
	        static-map (into {} (for [k static-fields] ;establish a remapping from k 
	                                 [k (get bridgemap k)])) 
	        re-map (fn [k] 
	                    (let [[tgt _] (get bridgemap k)]
	                      [tgt (get m k)]))]    
	    (into fieldmap 
	          (for [k (keys bridgemap)] ;preserves original order...
	              (if (static-fields k) 
	                 (get static-map k)
	                 (re-map k))))))
  ([bridgemap m] (translate-map 
                   bridgemap (field-ordered-map (vals bridgemap)) m)))
(defn translate-maps
  "Translate multiple maps or records using a bridge.  More effecient than
   individual calls to translate-map.  Returns a sequence of maps according
   to the bridgemap specification."
  [bridgemap coll]
  (let [fieldmap (field-ordered-map (vals bridgemap))
        translatef (fn [m] (translate-map bridgemap fieldmap m))]
    (map translatef coll)))

(defn translate-map2
  "Given a bridging specified by bridgemap, and a source map m, returns the 
   result of remapping m's keys to those specified in the bridgemap.  The 
   resulting map's keys include only the bridge keys.  For fields not bridged 
   during the translation, default values are drawn from the bridgemap."
  [bridgemap m]  
  (let [fields (into #{} (keys m)) ;possible source keys
        bridges (into #{} (keys bridgemap)) ;all keys we're going to produce
        mapped-fields (setlib/intersection fields bridges) ;keys we can directly produce
        ;if k exists in both fields and bridges, return [(first (get bridgemap k)) v]
        ;if k does not exist in both fields and bridges...
        ;  return (get bridgemap k)
        static-fields (setlib/difference bridges mapped-fields) ;keys we have to remap using defaults
        static-map (into {} (for [k static-fields] ;establish a remapping from k 
                                 [k (get bridgemap k)])) 
        re-map (fn [k] 
                    (let [[tgt _] (get bridgemap k)]
                      [tgt (get m k)]))]    
    (for [k (keys bridgemap)] ;preserves original order...
               (if (static-fields k) 
                   (get static-map k)
                   (re-map k)))))

(comment
  '(Sample Usage)
  ;use low level add-bridge to directly register a bridge  
	(add-bridge :sample
						  {"QTY" "Quantity"  
						   "Start" "StartDay"  
						   "Duration" "Duration"
						   "TAA SRC" "SRC" 
						   "Mission" "DemandGroup"
						   "Operation" "Vignette"
						   "COA" "Category"
						   "Category" "Bin"})
	;derive a subset of an existing bridgemap using normal sequence operations.
	(def sample-minus (into {} (drop 3 (get-bridge :sample)))) 
  
  ;define a template for a bridge as vector of triples.
	(def bridge-template 
	  [["QTY" "Quantity" 0]  
	   ["Start" "StartDay" 0]  
	   ["Duration" "Duration" 0]
	   ["TAA SRC" "SRC" NA] 
	   ["Mission" "DemandGroup" NA]
	   ["Operation" "Vignette" NA]
	   ["COA" "Category" NA]
	   ["Category" "Bin" NA]])
	
  (def bridge-template2
	  [[:qty "Quantity" 0]  
	   ["Start" "StartDay" 0]  
	   [:duration "Duration" 0]
	   ["TAA SRC" "SRC" NA] 
	   ["Mission" "DemandGroup" NA]
	   [:operation "Vignette" NA]
	   ["COA" "Category" NA]
	   ["Category" "Bin" NA]])
  
  (def bridge-template3
	  [["Start" "StartDay" 0]  
	   [:duration "Duration" 0]
	   ["TAA SRC" "SRC" NA] 
	   ["Mission" "DemandGroup" NA]
	   [:operation "Vignette" NA]
	   ["COA" "Category" NA]
	   ["Category" "Bin" NA]
     [:qty "Quantity" 0]])
  
  ;use high-level (preferred) defbridge macro to create a bridge called 
  ;samplebridge, according to the aforementioned template.
	(defbridge samplebridge bridge-template)
  (defbridge samplebridge2 bridge-template2)
  (defbridge samplebridge3 bridge-template3)
	
	(def testdata 
	  {"Operation" "Blau"
	   "Start"     0
	   "TAA SRC"   "01234A333"
	   "Mission"   "Anonymous"})
  (def testdata2
	  {"Start"     99
     :operation "Blau"	   
	   "TAA SRC"   "01234A333"
	   "Mission"   "Anonymous"
     :qty 10})
  ;translate the testdata using the bridge we just created.  Note, samplebridge
  ;is a bound var - defbridge did that for us - so we can use it. 
  (translate-map samplebridge testdata)
  ;or we can use lower-level functions.
  (translate-map (get-bridge :samplebridge) testdata)
  ;or we can inline it
  (translate-map (make-bridge bridge-template) testdata)
)

;;Up and coming functionality....trying to develop a higher order "roll" 
;;function to more easily define the rolling-up of structure.
;(defbridge BAF-supply-bridge
;  [["SRC"]
;   [:group ["AC" "RC"] :as "Component"]
;   [:group ["AC STR" "RC STR"] :as "STR" :by :sum]
;   ["Pool" :as "MyPool"]])

;  [["SRC" "SRC" nil]
;     ["AC" "Component"]
;       ["RC" "Component"]
;     ["AC STR" "STR"]
;       ["RC STR" "STR"]
;   ["Pool"]]

;  [["SRC"]   
;   [["AC" "Component"]
;    ["AC STR" "STR"]]
;   [["RC" "Component"]
;    ["RC STR" STR"]]
;   ["Pool"]]

;  [["Component" :sum "QTY"]      [["AC"      "RC"] 
;   [:sum "STR"]    ]        ["AC STR"  "RC STR"]]
;                              
;  = 
;  [["Component" "AC"]
;   ["Component" "RC"]]
;  [["STR "AC STR"]
;   ["STR" "RC STR"]]

;(roll ["Component" "QTY"] from ["AC" "RC"] using :sum)
;
;
;(defn roll
;  "Produces a sequence of mappings that will create a new field 
;   called rollname, with a default value of each of the fields in 
;   flds.  Also creates a new field called valname, with each of the values from
;   the rolled fields."
;  [[rollname valname] & flds]
;  (reduce 
;    #(reduce conj %1
;       [[%2 rollname %2] [%2 valname]]) [] flds))



;["RC"] = [["Component" "RC"] ["QTY" (val "RC")]]