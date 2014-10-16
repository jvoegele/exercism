(ns meetup
  (:use [clojure.string :only [upper-case]])
  (:import (java.util Calendar)))

(defn- valid-month? [month] (and (> month 0) (<= month 12)))

(defn- calendar-field
  "Returns the value of the Calendar field with the given name."
  [field-name]
  (eval `(. Calendar ~(symbol (upper-case (name field-name))))))

(defn ordinal-count
  "Returns the numeric count for the given ordinal."
  [ordinal]
  ({"first" 1, "second" 2, "third" 3, "fourth" 4} (name ordinal) 0))

(defn- calendar
  ([year month] (calendar year month 1))
  ([year month day-of-month]
    {:pre [(valid-month? month)]}
    (let [cal (Calendar/getInstance)]
      (.set cal year (dec month) day-of-month)
      cal)))

(defn- year-month-day
  "A vector of the year, month, and day for the given calendar."
  [cal]
  [(.get cal Calendar/YEAR)
   (inc (.get cal Calendar/MONTH))
   (.get cal Calendar/DAY_OF_MONTH)])

(defn- add-day
  ([cal] (add-day cal 1))
  ([cal n]
    (.add cal Calendar/DAY_OF_MONTH n)
    cal))

(defn- move-to-day-of-week
  ([cal day-of-week]
    (move-to-day-of-week cal day-of-week :forward))
  ([cal day-of-week direction]
    (let [day (calendar-field day-of-week)
          n (if (= direction :backward) -1 1)]
      (while (not= (.get cal Calendar/DAY_OF_WEEK) day)
        (add-day cal n))
      cal)))

(defn- go-to-last-day-of-month [cal]
  (.set cal Calendar/DAY_OF_MONTH 1)
  (.add cal Calendar/MONTH 1)
  (.add cal Calendar/DAY_OF_MONTH -1)
  cal)

(defn nth-day [year month day ordinal]
  (loop [cal (calendar year month)
         i 0
         n (ordinal-count ordinal)]
    (if (>= i n)
      (year-month-day cal)
      (do (when (> i 0) (add-day cal))
          (move-to-day-of-week cal day)
          (recur cal (inc i) n)))))

(defn last-day [year month day]
  (let [cal (go-to-last-day-of-month (calendar year month))]
    (year-month-day (move-to-day-of-week cal day :backward))))

(defn teenth-day [year month day]
  (let [cal (calendar year month 13)]
    (year-month-day (move-to-day-of-week cal day))))


(def days [:sunday
           :monday
           :tuesday
           :wednesday
           :thursday
           :friday
           :saturday])

(def teenth-days [:sunteenth
                  :monteenth
                  :tuesteenth
                  :wednesteenth
                  :thursteenth
                  :friteenth
                  :saturteenth])

(doseq [[day teenth-day-name] (zipmap days teenth-days)]
  (intern *ns* (symbol (name teenth-day-name))
    (fn [m y] (teenth-day y m day))))

(doseq [day days]
  (intern *ns* (symbol (str "last-" (name day)))
    (fn [m y] (last-day y m day))))

(doseq [day days
        ordinal ["first" "second" "third" "fourth"]]
  (let [fn-name (str ordinal "-" (name day))]
    (intern *ns* (symbol fn-name)
      (fn [m y] (nth-day y m day ordinal)))))


