(defproject spork "0.2.0.6-SNAPSHOT"
  :description
  "A set of libraries derived from Spoon's Operations Research Kit.
   Libraries are modular and will be supported as stand-alone dependencies.
   This project is an uberjar that includes all of the modular bits in a single
   jar, for easier dependencies.  spork is purpose-built to be clojure-centric,
   focusing on a functional API, a default towards purity and persistent data
   structures, with alternative mutable/imperative counterparts.  The vast
   majority of spork is written in Clojure, with an intentional nod toward
   independence from external libraries, particularly Java libs.  Consequently,
   spork should provide a lightweight, clojure-based platform for the topics
   covered by the libraries.  I am currently working to eliminate legacy
   java dependencies where possible."
  :url "None Currently"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure  "1.8.0"]
                 [clj-tuple            "0.2.2"]
                 [net.jpountz.lz4/lz4    "1.3"] ;lz4 compression test.
                 [net.sf.jung/jung-api           "2.0.1"]
                 [net.sf.jung/jung-graph-impl    "2.0.1"]
                 [net.sf.jung/jung-algorithms    "2.0.1"]
                 [net.sf.jung/jung-visualization "2.0.1"]
                 [net.sf.jung/jung-io            "2.0.1"]
                 [org.apache.poi/poi             "3.9"]
		 [org.apache.poi/poi-ooxml       "3.9"]
                 [org.apache.poi/poi-scratchpad  "3.9"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/data.avl   "0.0.13"]
                 [iota "1.1.3"]
                 [org.clojure/core.rrb-vector  "0.0.11"]
                 ;;serialization lib.
                 [com.taoensso/nippy "2.11.0-RC1"] 
                 [ctries.clj "0.0.3"]
                ;experimental dependencies, ephemeral.
                 
                ;[org.clojure/data.finger-tree "0.0.2"]
                ;[primitive-math         "0.1.3"]
                ;[org.clojure/core.match "0.2.1"]
                ;[immutable-int-map    "0.1.0"]
                
;                 [clojure-watch "LATEST"] ;ephemeral dependency.
                 ]
  :aot [spork.cljgui.components.PaintPanel]
  :profiles {:publish [:uberjar
                       {:aot [;spork.cljgui.components.PaintPanel
                              spork.cljgui.components.swing
                              spork.util.table]}]
             :install {:aot [;spork.cljgui.components.PaintPanel
                             spork.cljgui.components.swing
                             spork.util.table]}
             ;;due to problems with AOT, and an effort to GET
             ;;everything to AOT, I had to use an approach derived
             ;;from lein-aot-order, specifically using their code
             ;;[since the plugin failed for me..] using
             ;;clojure.tools.namespace to derive a load-order.
             ;;From there, I manually pushed classes that
             ;;must be AOT'd up to the top of the dependency
             ;;tree (PaintPanel is the only one in this instance..)
             ;;Then, I incrementally 'lein with-profile order compile
             ;;correcting errors, identifying namespaces to prune along
             ;;the way.  I either fixed, or non-aot'd the namespaces
             ;;in the order vector.
             :order  {:aot [spork.cljgui.components.PaintPanel
                            spork.util.datetime
                            spork.opt.packing
                            spork.util.vectors
                            spork.util.vecmath
                            spork.protocols.spatial
                            spork.graphics2d.font
                            spork.graphics2d.swing.shared
                            spork.graphics2d.canvas
                            spork.graphics2d.swing
                            spork.util.io
                            spork.util.zipfile
                            spork.util.general
                            spork.graphics2d.image
                            spork.geometry.shapes
                            spork.graphics2d.stroke
                            spork.graphics2d.scene
                            spork.util.metaprogramming
                            spork.events.observe
                            spork.events.base
                            spork.events.native
                            spork.mvc
                            spork.cljgui.components.swing
                            spork.sketch
                            spork.sketch.activity
                            spork.util.combinatoric
                            spork.util.ranges
                            spork.util.numerics
                            spork.util.stats
                            spork.util.sampling
                            ;spork.examples.samplingvis
                            spork.data.sptree
                            spork.util.excel.docjure
                            spork.data.passmap
                            spork.entitysystem.store
                            ;spork.entitysystem.examples.orcbattle
                            spork.util.tags
                            ;spork.sim.discrete
                            spork.data.randq
                            spork.protocols.core
                            spork.util.eager
                            spork.data.protocols
                            spork.data.mutable
                            spork.data.orderedmap
                            spork.data.digraph
                            spork.data.priorityq
                            spork.data.fringe
                            spork.data.mpq
                            spork.data.searchstate
                            spork.cljgraph.search
                            spork.cljgraph.core
                            spork.cljgraph.jungapi
                            spork.cljgraph.io
                            ;spork.incanter.extensions
                            spork.util.generators
                            spork.util.reducers
                            spork.util.array
                            spork.data.tables
                            
                            spork.opt.core
                            spork.util.string
                            ;spork.entitysystem.systems
                            spork.util.diff
                            spork.util.collections
                            spork.data.ctrie
                            spork.util.indexed
                            ;spork.sketch.legacy
                            ;spork.util.help
                            spork.graphics2d.strokes
                            spork.util.topotree
                            spork.cljgraph.flow
                            spork.cljgraph.networksimplex
                            spork.opt.representation
                            spork.ai.behavior
                            spork.ai.core
                            spork.ai.demo
                            spork.util.stream
                            spork.sim.data
                            spork.sim.pure.network
                            spork.sim.updates
                            spork.data.cell
                            spork.sim.agenda
                            spork.opt.annealing
                            spork.opt.optimization
                            spork.opt.neighborhood
                            spork.sim.pure.test
                            spork.util.clipboard
                            spork.util.bridging
                            spork.util.comparison
                            spork.util.mailbox
                            spork.data.edge
                            spork.data.managed
                            ;spork.cljgraph.main
                            spork.data.grid
                            spork.sim.simcontext
                            spork.sim.engine
                            spork.async.system
                            spork.ai.behaviorcontext
                            spork.data.associative
                            spork.data.quadtree
                            spork.data.simpleq
                            spork.util.temporal
                            spork.trends
                            spork.util.vector
                            spork.util.inspection
                            spork.ai.messaging
                            spork.util.record
                            spork.data.sparse
                            spork.util.parsing
                            spork.entitysystem.ephemeral
                            spork.util.table
                            spork.sim.core
                            spork.documentation
                            spork.cljgraph.arrayflow
                            spork.entitysystem.entitytesting
                            spork.util.xml
                            ;;This exists in backburner...remove!
                            ;spork.sim.events
                            spork.graphics2d.debug
                            spork.util.cellular
                            spork.graphics2d.primitive
                            spork.data.indexed
                            spork.util.bitset
                            spork.opt.gaops
                            spork.cljgraph.flowtests
                            spork.ai.testing
                            spork.entitysystem.diff
                            spork.util.serial
                            spork.sim.history
                            spork.cljgraph.tests
                            spork.sim.examples.westworld
                            spork.opt.dumbga
                            spork.data.adjacency
                            spork.mining.core
                            spork.sim.test
                            spork.opt.dumbanneal
                            spork.data.test
                            spork.util.excel.core
                            spork.util.zip
                            ;;Not necessary
                            ;spork.cljgui.components.CanvasGraphics
                            ;;everything after this is unused...
                            
                            ;spork.util.processor
                            ;spork.mining.clustering
                            spork.data.indexedgraph
                            ;spork.DEVS.deprecated
                            ;spork.opt.displaced
                            spork.util.interop
                            spork.data.persistentq
                            ;spork.ai.notes
                            ;spork.sim.impure.core
                            ;spork.ai.logic
                            ;spork.entitysystem.examples.orcbattleLOL
                            spork.ai.machine
                            spork.data.bitset
                            ;spork.opt.fastmultipole
                            spork.data.lazymap
                            spork.data.core
                            ;spork.entitysystem.examples.oblol
                            ;spork.util.log
                            ;spork.data.depq
                            ]}
             }
  :jvm-opts ^:replace ["-Xmx1g" "-XX:NewSize=200m"])
