(ns spork.util.datetime)

(defn simple-date [year month day]
  (java.util.Date. (- year 1900) (dec month) day))

(defn get-date []
  (java.util.Date.))

(defn date->num [date]
  (Integer/parseInt(str (+ 1900 (.getYear date)) (inc (.getMonth date)) 
                        (.getDate date))))
(defn date->time[date]
  (.getTime date))

(defn num->date [num]
  (java.util.Date. num))

(defn daystream 
  ([weekday] (daystream weekday (java.util.Date.)))
  ([weekday ^java.util.Date startdate]
  "Find a stream of weekdays, where weekdays are proper names for days of 
   the week.  Added this due to Rick Hanson's relentless jibes.  Produces an 
   infinite sequence of monotonically increasing dates."
  (let [daysofweek (zipmap (range 1 8) ["Sunday" "Monday" "Tuesday" 
                         "Wednesday" "Thursday" "Friday" "Saturday"])
        dayfield java.util.GregorianCalendar/DAY_OF_WEEK
        currday (fn [cal] (.get cal dayfield))
        cal (doto (java.util.GregorianCalendar.) (.setTime startdate))
        get-weekday (fn [cal] (get daysofweek (currday cal)))
        next-weekday (fn [cal] (do (.add cal dayfield 7) cal))
        find-nextweekday (fn [cal] (do 
                                     (while (not= (get-weekday cal) weekday)
                                      (do (.add cal dayfield 1)))
                                     cal))]
      (letfn [(stream [wd c] 
                (if (not= (get-weekday c) wd)                       
                  (stream wd (find-nextweekday c))               
                  (cons (.getTime c)  
                     (lazy-seq (stream weekday (next-weekday c))))))]
             (stream weekday cal)))))


