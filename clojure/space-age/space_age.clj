(ns space-age)

(def ^:const earth-year 31557600)

(defmacro def-on-planet
  "Macro that defines an `on-<planet>` function given a planet name and year length."
  [planet year-length]
  (let [fn-name (symbol (str "on-" (name planet)))]
    `(defn ~fn-name [seconds#]
       (/ (double seconds#) (* ~earth-year ~year-length)))))

(def-on-planet mercury 0.2408467)
(def-on-planet venus 0.61519726)
(def-on-planet earth 1.0)
(def-on-planet mars 1.8808158)
(def-on-planet jupiter 11.862615)
(def-on-planet saturn 29.447498)
(def-on-planet uranus 84.016846)
(def-on-planet neptune 164.79132)
