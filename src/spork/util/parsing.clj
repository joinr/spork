(ns spork.util.parsing)

;;Note -> this isn't quite so hot, but it's a general failsafe.
;;It's much much better to use a standard parser for each field, if the 
;;field is a known type.

(defn parse-string 
	"Parses a string, trying various number formats.  Note, scientific numbers,
	 or strings with digits sandwiching an E or an e will be parsed as numbers,
	 possibly as Double/POSITIVE_INFINITY, i.e. Infinity."
	[^String value]	
  (try (Integer/parseInt value)
    (catch NumberFormatException _
      (try (Double/parseDouble value)
        (catch NumberFormatException _ value)))))

(def scientific-reg 
	"A regular expression gleefully borrowed from Stack Overflow.  Matches 
	 strings that correspond to scientific numbers."
	#"-?\d*\.?\d+[Ee][+-]?\d+")
	 
(defn parse-string-nonscientific 
	"Parses a string, trying various number formats.  Scientific numbers,
	 or strings with digits sandwiching an E or an e will be kept as strings.  
	 Helpful in contexts where there are alphanumeric string values."
	[^String value]
	(if-let [res (re-find scientific-reg value)]
		value
		(parse-string value)))

;;a schema is just a map of field names to either keys or custom parse
;;functions.

;(def the-schema {:field1 :long :field2 :long :field3 :long :field4
;:double}

(defn get-key-or-string [m k default]
  (get m k
       (get m (if (keyword? k)
                (str (subs (str k) 1))
                (keyword k)) default)))

;;This should be moved to a spork.parse lib, since it's 
;;pretty general purpose.
;;Default parsers for a number of data types.
;;string parsers
(def parse-defaults 
  {:string identity
   :text   identity
   :number (fn [^String x] (try (Integer/parseInt x)
                                 (catch NumberFormatException _
                                   (Double/parseDouble x))))
   :float  (^double fn [^String x] (Double/parseDouble x))
   :int    (^int fn [^String x]   (Integer/parseInt x))
   :long   (^int fn [^String x]   (Long/parseLong x))
   :date   (^java.util.Date fn [^String x] (java.util.Date. x))})


(defn parsing-scheme 
  "A parsing scheme is designed to associate a set of parsers with possibly 
   named fields in a table or record.  When parsing values, the field-parser 
   map is consulted to determine if a parser is defined for the field; If not, 
   the optional default-parser is used.  The standard default-parser is to 
   parse any input as an int, or a float, or a string.  This is slow, but 
   general."
  [field-parser & {:keys [default-parser] 
                                      :or   {default-parser parse-string}}]
  (let [get-parser (memoize (fn [field] 
                              (if-let [pfunc (get-key-or-string field-parser field default-parser)]
                                (cond (keyword? pfunc) (get parse-defaults pfunc default-parser) 
                                      (fn? pfunc) pfunc)
                                parse-string)))]
    (fn [field ^String v] ((get-parser field) v))))

(defn nested-parser 
  "A nested-parser allows us to compose a sequence of parsers; where 
   the parsers will be tried, left-to-right.  If the left parse fails, 
   its default parse is the right, which either succeeds or defaults to 
   the parse to its right, eventually ending with the final parser."
  [schemes & {:keys [default-parser] 
                                :or   {default-parser parse-string}}]
  (let [revschemes (reverse schemes)]
    (reduce (fn [r l]
              (parsing-scheme l :default-parser r))
            (parsing-scheme (first revschemes) :default-parser default-parser)
            (rest revschemes))))

;;testing
(comment 




)
