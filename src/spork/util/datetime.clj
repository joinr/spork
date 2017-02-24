;;A very light wrapper around java's data and time functions, written back in
;;2012ish.

;;Arguably today, the Approved Solution, is to use clj-time, which wraps
;;joda-time and provides a more robust and tested way of dealing with time.

;;Marked as LEGACY
(ns spork.util.datetime
  (:import java.util.GregorianCalendar))

(defn simple-date [year month day]
  (java.util.Date. (- year 1900) (dec month) day))

(defn ^java.util.Date get-date [] (java.util.Date.))

(defn ^long date->num [^java.util.Date date]
  (Integer/parseInt (str (+ 1900 (.getYear date)) (inc (.getMonth date)) 
                        (.getDate date))))
(defn ^long date->time [^java.util.Date date]
  (.getTime date))

(defn ^java.util.Date num->date [num]
  (java.util.Date. (long num)))

(defn daystream 
  ([weekday] (daystream weekday (java.util.Date.)))
  ([weekday ^java.util.Date startdate]
  "Find a stream of weekdays, where weekdays are proper names for days of 
   the week.  Added this due to Rick Hanson's relentless jibes.  Produces an 
   infinite sequence of monotonically increasing dates."
  (let [daysofweek (zipmap (range 1 8) ["Sunday" "Monday" "Tuesday" 
                         "Wednesday" "Thursday" "Friday" "Saturday"])
        dayfield java.util.GregorianCalendar/DAY_OF_WEEK
        currday (fn [^GregorianCalendar cal] (.get cal dayfield))
        cal (doto (java.util.GregorianCalendar.) (.setTime startdate))
        get-weekday (fn [^GregorianCalendar cal] (get daysofweek (currday cal)))
        next-weekday (fn [^GregorianCalendar cal] (do (.add cal dayfield 7) cal))
        find-nextweekday (fn [^GregorianCalendar cal] 
                           (do 
                             (while (not= (get-weekday cal) weekday)
                               (do (.add cal dayfield 1)))
                             cal))]
      (letfn [(stream [wd ^GregorianCalendar c] 
                (if (not= (get-weekday c) wd)                       
                  (stream wd (find-nextweekday c))               
                  (cons (.getTime c)  
                     (lazy-seq (stream weekday (next-weekday c))))))]
             (stream weekday cal)))))


