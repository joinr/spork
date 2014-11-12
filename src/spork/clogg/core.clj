;;A clojure  grammar of graphics implementation.
;;Loosely based on julia's gadfly implementation.
(ns spork.clogg.core)


(declare emptystat emptygeom)

(defprotocol IElement)



;; type Layer <: Element
;;     data_source::Union(AbstractDataFrame, Nothing)
;;     mapping::Dict
;;     statistic::StatisticElement
;;     geom::GeometryElement
;;     theme::Union(Nothing, Theme)

;;     function Layer()
;;         new(nothing, Dict(), Stat.nil(), Geom.nil(), nothing)
;;     end

;;     function Layer(lyr::Layer)
;;         new(lyr.data_source,
;;             lyr.mapping,
;;             lyr.statistic,
;;             lyr.geom,
;;             lyr.theme)
;;     end
;; end


(defrecord Layer [data-source mapping statistic geom theme]
  IElement)





;;In julia, these are equivalent to keyword args....
;;wierd...

;; function layer(data_source::AbstractDataFrame, elements::ElementOrFunction...;
;;                mapping...)
;;     lyr = Layer()
;;     lyr.data_source = data_source
;;     lyr.mapping = clean_mapping(mapping)
;;     lyrs = Layer[lyr]
;;     for element in elements
;;         add_plot_element(lyrs, element)
;;     end
;;     lyrs
;; end

;; function layer(elements::ElementOrFunction...; mapping...)
;;     lyr = Layer()
;;     lyr.mapping = clean_mapping(mapping)
;;     lyrs = Layer[lyr]
;;     for element in elements
;;         add_plot_element(lyrs, element)
;;     end
;;     lyrs
;; end


(defn ->layer 
  "Constructs a layer.  A plot has zero or more layers. Layers have a particular geometry and their
   own data, which is inherited from the plot if not given."
  ([] (Layer. nil {} emptystat emptygeom nil))
  ([^Layer lyr] (Layer. (.data-source lyr)
                        (.mapping lyr)
                        (.statistic lyr)
                        (.geom lyr)
                        (.theme lyr)))
  ([elements mapping] (->layer nil elements mapping))
  ([data-source elements  mapping]  
     (reduce add-plot-element 
             (merge (->layer)
                    :data-source data-source
                    :elements    elements)
             elements)))


(defn update-last [v f]
  (let [idx (unchecked-dec (count v))]
    (assoc v idx (f (nth v idx)))))


;;layers are really vectors anyway....        
        
(defn plot-type [arg] 
  (if (fn? arg) :function 
      (type arg)))



(defn plot-dispatch 
  ([lyrs arg]      [(plot-type lyrs) (plot-type arg)])
  ([lyrs itm arg]  [(plot-type lyrs) (plot-type itm) (plot-type arg)]))


;;do we have a match for the type? 
                      
;;Multimethods are slow....we'll look at changing this at some point
;;if performance matters.
(defmulti add-plot-element  plot-dispatch)

(defmethod add-plot-element [Layer Element] 
  (throw (Exception. "Elements are bad!")))

;;Lame...we don't need to do this...
;; function add_plot_element(lyrs::Vector{Layer}, arg::GeometryElement)
;;     if ! is(lyrs[end].geom, Geom.nil())
;;         push!(lyrs, copy(lyrs[end]))
;;     end
;;     lyrs[end].geom = arg
;; end


;;We can continually build the match up....
;;Maintain the expr of the matcher, and 
;;recompile everytime we add a dispatch? 

(match [[t1 t2]]
   [Layer GeometryElement] ....
   [Layer ...])

(defprotocol IMatcher
  (match-fn [m])
  (add-match [m clause])
  (match-expr [m]))

(declare as-match) 

(deftype Matcher [args ^:volatile-mutable matches ^:volatile-mutable else ^:volatile-mutable matchfn ^:volatile-mutable fresh]
  IMatcher 
  (match-fn [m] (if fresh matchfn 
                    (do (set! matchfn (eval `(fn ~args ~(match-expr m))))
                        (set! fresh true)
                        matchfn)))
  (match-expr [m] (as-match args matches else))
  (add-match [m [from to]] (do (set! fresh false)
                               (set! matches 
                                     (-> matches (conj from) (conj to)))
                               m)))                               

(defmacro ->matcher [args matches & [else]]
  `(Matcher. (quote [~@args]) (quote [~@matches]) (quote ~else) nil nil))

(defn as-match [args matches else]
  `(match ~args
                 ~@matches
                 :else ~else))

(defn unzip [xs]
  (reduce (fn [acc [x y]]
            (-> acc (conj x) (conj y)))
          [] xs))
;;we can now define match-functions that are extensible...
;;so a type-based matcher looks like this....
(defmacro matcher-by [name f args]
  `(let [m# (->matcher ~args nil (throw (Exception. "no match!")))]
    (vary-meta (fn ~name ~args
                 (let [~@(unzip (for [a args]
                                    `[~a (~f ~a)]))]
                   ((match-fn m#) ~@args)))
               merge {:type-dispatcher (quote ~name)
                      :arity ~(count args)
                      :matcher m#})))


(defmulti add-plot-element [Layer GeometryElement] [lyrs arg] 
  (conj lyrs (assoc (last lyrs :geom arg))))    

;; function add_plot_element(lyrs::Vector{Layer}, arg::Base.Callable)
;;     add_plot_element(lyrs::Vector{Layer}, arg())
;; end

(defmulti add-plot-element [Layer :function] [lyrs arg]
  (add-plot-element lyrs (arg)))

;;statistics are applied to all layers...they're shared. (dunno if
;;this is wise...)
;; function add_plot_element(lyrs::Vector{Layer}, arg::StatisticElement)
;;     [lyr.statistic = arg for lyr in lyrs]
;; end

(defmulti add-plot-element [Layer StatisticElement] [lyrs statistic]
  (mapv (fn [l] (assoc l :statistic statistic)) lyrs))
  

;;themes are applied to all layers...they're shared.
;; function add_plot_element(lyrs::Vector{Layer}, arg::Theme)
;;     [lyr.theme = arg for lyr in lyrs]
;; end

(defmulti add-plot-element [Layer Theme] [lyrs arg]
  (mapv (fn [l] (assoc l :theme arg)) lyrs))


# A full plot specification.
;; type Plot
;;     layers::Vector{Layer}
;;     data_source::Union(Nothing, AbstractDataFrame)
;;     data::Data
;;     scales::Vector{ScaleElement}
;;     statistics::Vector{StatisticElement}
;;     coord::Union(Nothing, CoordinateElement)
;;     guides::Vector{GuideElement}
;;     theme::Theme
;;     mapping::Dict

;;     function Plot()
;;         new(Layer[], nothing, Data(), ScaleElement[], StatisticElement[],
;;             nothing, GuideElement[], default_theme)
;;     end
;; end

(defrecord Plot [layers data-source data scales statistics coord guides theme mapping])

(defn ->plot 
  ([] (Plot. [] nil (->data) [] [] nil [] default-theme)))




;;let's define a defgeneric method that uses core.match....

(require '[clojure.core.match :as match])

  

(defmulti add-plot-element [Plot 
  
function add_plot_element(p::Plot, data::AbstractDataFrame, arg::Function)
    add_plot_element(p, data, arg())
end




function add_plot_element(p::Plot, data::AbstractDataFrame, arg::GeometryElement)
    if !isempty(p.layers) && isa(p.layers[end].geom, Geom.Nil)
        p.layers[end].geom = arg
    else
        layer = Layer()
        layer.geom = arg
        push!(p.layers, layer)
    end
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::ScaleElement)
    push!(p.scales, arg)
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::StatisticElement)
    if isempty(p.layers)
        push!(p.layers, Layer())
    end

    p.layers[end].statistic = arg
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::CoordinateElement)
    p.coord = arg
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::GuideElement)
    push!(p.guides, arg)
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::Layer)
    push!(p.layers, arg)
end


function add_plot_element(p::Plot, data::AbstractDataFrame, arg::Vector{Layer})
    append!(p.layers, arg)
end


function add_plot_element{T <: Element}(p::Plot, data::AbstractDataFrame, f::Type{T})
    add_plot_element(p, data, f())
end


function add_plot_element(p::Plot, ::AbstractDataFrame, theme::Theme)
    p.theme = theme
end
