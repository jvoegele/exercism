(ns gigasecond
  (:import [java.util Calendar GregorianCalendar]))

(defn from [year month day]
  (let [cal (GregorianCalendar. year (dec month) day)
        billion 1000000000]
    (.add cal Calendar/SECOND billion)
    [(.get cal Calendar/YEAR)
     (inc (.get cal Calendar/MONTH))
     (.get cal Calendar/DAY_OF_MONTH)]))
